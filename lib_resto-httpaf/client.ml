(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Core
open Async

module Make (Encoding : Resto.ENCODING) = struct

  open Httpaf

  module Media_type = Media_type.Make(Encoding)
  module Service = Resto.MakeService(Encoding)

  type content_type = (string * string)
  type raw_content = [ `read ] Body.t * content_type option
  type content = [ `read ] Body.t * content_type option * Media_type.t option

  type ('o, 'e) generic_rest_result =
    [ `Ok of 'o option
    | `Conflict of 'e
    | `Error of 'e
    | `Forbidden of 'e
    | `Not_found of 'e
    | `Unauthorized of 'e
    | `Bad_request of string
    | `Method_not_allowed of string list
    | `Unsupported_media_type
    | `Not_acceptable of string
    | `Unexpected_status_code of Status.t * content
    | `Connection_failed of string
    | `OCaml_exception of string ]

  type ('o, 'e) service_result =
    [ ('o, 'e option) generic_rest_result
    | `Unexpected_content_type of raw_content
    | `Unexpected_content of (string * Media_type.t) * string
    | `Unexpected_error_content_type of raw_content
    | `Unexpected_error_content of (string * Media_type.t) * string ]

  module type LOGGER = sig
    type request
    val log_empty_request: Uri.t -> request Deferred.t
    val log_request:
      ?media:Media_type.t -> 'a Encoding.t ->
      Uri.t -> string -> request Deferred.t
    val log_response:
      request -> ?media:Media_type.t -> 'a Encoding.t ->
      Status.t -> string Deferred.t Lazy.t -> unit Deferred.t
  end

  type logger = (module LOGGER)

  let null_logger =
    (module struct
      type request = unit
      let log_empty_request = (fun _ -> Deferred.unit)
      let log_request = (fun ?media:_ _ _ _-> Deferred.unit)
      let log_response = (fun _ ?media:_ _ _ _ -> Deferred.unit)
    end : LOGGER)

  let timings_logger src =
    (module struct
      type request = string * float
      let log_empty_request uri =
        let tzero = Unix.gettimeofday () in
        return (Uri.to_string uri, tzero)
      let log_request ?media:_ _enc uri _body = log_empty_request uri
      let log_response (uri, tzero) ?media:_ _enc _code _body =
        let time = Unix.gettimeofday () -. tzero in
        Logs_async.info ~src (fun m -> m "Request to %s succeeded in %gs@." uri time)
    end : LOGGER)

  let faked_media = {
    Media_type.name = AnyMedia ;
    q = None ;
    pp = (fun _enc ppf s -> Format.fprintf ppf "@[<h 0>%a@]" Format.pp_print_text s) ;
    construct = (fun _ -> assert false) ;
    destruct = (fun _ -> assert false) ;
  }

  let full_logger src =
    (module struct
      module Log_async = (val Logs_async.src_log src : Logs_async.LOG)
      include Log_async
      let cpt = ref 0
      type request = int * Uri.t
      let log_empty_request uri =
        let id = !cpt in
        incr cpt ;
        info (fun m -> m ">>>>%d: %a@." id Uri.pp uri) >>= fun () ->
        return (id, uri)
      let log_request ?(media = faked_media) enc uri body =
        let id = !cpt in
        incr cpt ;
        info begin fun m ->
          m "@[<v 2>>>>>%d: %a@,%a@]@." id Uri.pp uri (media.pp enc) body
        end >>= fun () ->
        return (id, uri)
      let log_response (id, _uri) ?(media = faked_media) enc code body =
        Lazy.force body >>= fun body ->
        info begin fun m -> m "@[<v 2><<<<%d: %s@,%a@]@."
            id (Status.to_string code) (media.pp enc) body
        end
    end : LOGGER)

  let find_media received media_types =
    match received with
    | Some received ->
        Media_type.find_media received media_types
    | None ->
        match media_types with
        | [] -> None
        | m :: _ -> Some m

  type log = {
    log:
      'a. ?media:Media_type.t -> 'a Encoding.t -> Httpaf.Status.t ->
      string Deferred.t Lazy.t -> unit Deferred.t ;
  }

  let read_answer_body body =
    let buf = Bigbuffer.create 512 in
    let read_done = Ivar.create () in
    Httpaf.Body.schedule_read body ~on_eof:(Ivar.fill read_done)
      ~on_read:begin fun b ~off:pos ~len ->
        Bigbuffer.add_bigstring buf (Bigstring.sub_shared b ~pos ~len)
      end ;
    Ivar.read read_done >>| fun () ->
    Bigbuffer.contents buf

  let core_iovecs_of_iovecs iovecs =
    let q = Queue.create () in
    let len = List.fold_left iovecs ~init:0 ~f:begin fun a ({ Faraday.buffer; off=pos; len }) ->
        Queue.enqueue q (Iobuf.(of_bigstring ~pos ~len buffer |> Expert.to_iovec_shared)) ;
        a+len
      end in
    q, len

  let rec flush_req conn w =
    match Client_connection.next_write_operation conn with
    | `Write iovecs ->
        let q, len = core_iovecs_of_iovecs iovecs in
        Writer.schedule_iovecs w q ;
        Client_connection.report_write_result conn (`Ok len) ;
        flush_req conn w
    | `Yield ->
        Client_connection.yield_writer conn (fun () -> flush_req conn w) ;
    | `Close _ -> ()

  let read_response conn r =
    match Client_connection.next_read_operation conn with
    | `Close -> Deferred.unit
    | `Read -> begin
        Reader.read_one_chunk_at_a_time r
          ~handle_chunk:begin fun buf ~pos ~len ->
            let nb_read = Client_connection.read conn buf ~off:pos ~len in
            return (`Consumed (nb_read, `Need_unknown))
          end >>= function
        | `Stopped () -> assert false (* cannot happen *)
        | `Eof
        | `Eof_with_unconsumed_data _ ->
            Deferred.unit (* normal case *)
      end

  let request ?config ~error_handler ~response_handler req =
    let wrbody, conn =
      Client_connection.request ?config req ~error_handler ~response_handler in
    don't_wait_for @@ Async_uri.with_connection
      (Uri.of_string req.Request.target) begin fun _sock _tls r w ->
      flush_req conn w ;
      read_response conn r
    end ;
    wrbody

  let call_and_retry_on_502
      ?config
      ?(nb_attempts=10)
      ?(delay_f=(fun i -> Time_ns.Span.of_int_ms (i*100)))
      ?(body_f=Body.close_writer) log req =
    let rec inner attempt =
      let err_iv = Ivar.create () in
      let response_iv = Ivar.create () in
      let wrbody = request ?config req
          ~error_handler:(Ivar.fill err_iv)
          ~response_handler:(fun r b -> Ivar.fill response_iv (r, b)) in
      body_f wrbody ;
      Ivar.read response_iv >>= fun (response, ansbody) ->
      match response.status with
      | `Bad_gateway ->
          read_answer_body ansbody >>= fun ansbody ->
          let delay = delay_f attempt in
          log.log ~media:faked_media Encoding.untyped response.status
            (lazy (Format.kasprintf return
                     "Attempt number %d/10, will retry after %a seconds.\n\
                      Original body follows.\n\
                      %s"
                     attempt Time_ns.Span.pp delay ansbody)) >>= fun () ->
          if attempt >= nb_attempts then
            Deferred.Result.fail (response, ansbody)
          else
            Clock_ns.after delay >>= fun () ->
            inner (succ attempt)
      | _ ->
          Deferred.Result.return (response, ansbody) in
    inner 1

  let process_response ?accept = function
    | Error (_, msg) -> return (`Bad_request msg)
    | Ok ({ Response.status; headers; _ }, (ansbody : [`read] Body.t)) ->
        let media_name =
          match Headers.get headers "content-type" with
          | None -> None
          | Some s ->
              match Utils.split_path s with
              | [x ; y] -> Some (x, y)
              | _      -> None (* ignored invalid *) in
        let media =
          match accept with
          | None -> None
          | Some media_types -> find_media media_name media_types in
        match status with
        | `OK -> return (`Ok (Some (ansbody, media_name, media)))
        | `No_content -> return (`Ok None)
        | #Status.t as status ->
            read_answer_body ansbody >>= fun msg ->
            match status with
            | `Created ->
                (* TODO handle redirection ?? *)
                failwith "Resto_cohttp_client.generic_json_call: unimplemented"
            | `Unauthorized -> return (`Unauthorized (ansbody, media_name, media))
            | `Forbidden -> return (`Forbidden (ansbody, media_name, media))
            | `Not_found -> return (`Not_found (ansbody, media_name, media))
            | `Conflict -> return (`Conflict (ansbody, media_name, media))
            | `Internal_server_error ->
                if media_name = Some ("text", "ocaml.exception") then
                  return (`OCaml_exception msg)
                else
                  return (`Error (ansbody, media_name, media))
            | `Method_not_allowed ->
                let allowed = Headers.get_multi headers "accept" in
                return (`Method_not_allowed allowed)
            | `Unsupported_media_type -> return  `Unsupported_media_type
            | `Not_acceptable -> return (`Not_acceptable msg)
            |  code -> return (`Unexpected_status_code (code, (ansbody, media_name, media)))

  let fold_hdrs header value headers =
    let header = String.lowercase header in
    if header <> "host" && (String.length header < 2 || String.sub header ~pos:0 ~len:2 <> "x-") then
      invalid_arg
        "Resto_cohttp.Client.call: \
         only headers \"host\" or starting with \"x-\" are supported" ;
    Headers.add headers header value

  let internal_call (log : log) ?accept ?body_f ?media req : (content, content) generic_rest_result Deferred.t =
    let headers =
      Headers.fold ~init:Headers.empty req.Request.headers ~f:fold_hdrs in
    let headers = Option.value_map media ~default:headers
        ~f:(fun media -> Headers.add headers "content-type" (Media_type.name media)) in
    let headers = Option.value_map accept ~default:headers
        ~f:(fun ranges -> Headers.add headers "accept" (Media_type.accept_header ranges)) in
    Monitor.try_with ~extract_exn:true begin fun () ->
      call_and_retry_on_502 ?body_f log { req with headers } >>=
      process_response ?accept
    end >>= function
    | Ok v -> return v
    | Error exn -> begin
        let msg =
          match exn with
          | Unix.Unix_error (e, _, _) -> Sexp.to_string (Unix.Error.sexp_of_t e)
          | Failure msg -> msg
          | Invalid_argument msg -> msg
          | e -> Exn.to_string e in
        return (`Connection_failed msg)
      end

  let handle_error log service (body, media_name, media) status f =
    if Body.is_closed body then
      log.log Encoding.untyped status (lazy (return "")) >>= fun () ->
      return (f None)
    else
      match media with
      | None ->
          return (`Unexpected_error_content_type (body, media_name))
      | Some media ->
          read_answer_body body >>= fun body ->
          let error = Service.error_encoding service in
          log.log ~media error status (lazy (return body)) >>= fun () ->
          match media.Media_type.destruct error body with
          | Ok body -> return (f (Some body))
          | Error msg ->
              return (`Unexpected_error_content ((body, media), msg))

  let prepare (type i)
      media_types ?(logger = null_logger) ?base
      (service : (_,_,_,_,i,_,_) Service.t) params query body =
    let module Logger = (val logger : LOGGER) in
    let media =
      match Media_type.first_complete_media media_types with
      | None -> invalid_arg "Resto_cohttp_client.call_service"
      | Some (_, m) -> m in
    let { Service.meth ; uri ; input } =
      Service.forge_request ?base service params query in
    begin
      match input with
      | Service.No_input ->
          Logger.log_empty_request uri >>= fun log_request ->
          return (None, None, log_request)
      | Service.Input input ->
          let body = media.Media_type.construct input body in
          Logger.log_request ~media input uri body >>= fun log_request ->
          return (Some (fun r -> Body.write_string r body),
                  Some media,
                  log_request)
    end >>= fun (body, media, log_request) ->
    let log = { log = fun ?media -> Logger.log_response log_request ?media } in
    return (log, meth, uri, body, media)

  let httpaf_meth_of_meth = function
    | `PATCH -> `Other "PATCH"
    | `DELETE -> `DELETE
    | `GET -> `GET
    | `POST -> `POST
    | `PUT -> `PUT

  let call_service media_types
      ?logger ?headers ?base service params query body =
    prepare media_types ?logger
      ?base service params query body >>= fun (log, meth, uri, body_f, media) ->
    let req =
      Request.create ?headers (httpaf_meth_of_meth meth) (Uri.path_and_query uri) in
    begin
      internal_call log ~accept:media_types ?body_f ?media req >>= function
      | `Ok None ->
          log.log Encoding.untyped `No_content (lazy (return "")) >>= fun () ->
          return (`Ok None)
      | `Ok (Some (body, media_name, media)) -> begin
          match media with
          | None ->
              return (`Unexpected_content_type (body, media_name))
          | Some media ->
              read_answer_body body >>= fun body ->
              let output = Service.output_encoding service in
              log.log ~media output `OK (lazy (return body)) >>= fun () ->
              match media.destruct output body with
              | Ok body -> return (`Ok (Some body))
              | Error msg ->
                  return (`Unexpected_content ((body, media), msg))
        end
      | `Conflict body ->
          handle_error log service body `Conflict (fun v -> `Conflict v)
      | `Error body ->
          handle_error log service body `Internal_server_error (fun v -> `Error v)
      | `Forbidden body ->
          handle_error log service body `Forbidden (fun v -> `Forbidden v)
      | `Not_found body ->
          handle_error log service body `Not_found (fun v -> `Not_found v)
      | `Unauthorized body ->
          handle_error log service body `Unauthorized (fun v -> `Unauthorized v)
      | `Bad_request _
      | `Method_not_allowed _
      | `Unsupported_media_type
      | `Not_acceptable _
      | `Unexpected_status_code _
      | `Connection_failed _
      | `OCaml_exception _ as err -> return err
    end >>= fun ans ->
    return (meth, uri, ans)

  let call_streamed_service media_types
      ?logger ?headers ?base service ~on_chunk ~on_close params query body =
    prepare
      media_types ?logger ?base
      service params query body >>= fun (log, meth, uri, body_f, media) ->
    begin
      let req =
        Request.create ?headers (httpaf_meth_of_meth meth) (Uri.path_and_query uri) in
      internal_call log ~accept:media_types ?body_f ?media req >>= function
      | `Ok None ->
          on_close () ;
          log.log Encoding.untyped `No_content (lazy (return "")) >>= fun () ->
          return (`Ok None)
      | `Ok (Some (body, media_name, media)) -> begin
          match media with
          | None -> return (`Unexpected_content_type (body, media_name))
          | Some media ->
              let buffer = Bigbuffer.create 2048 in
              let output = Service.output_encoding service in
              let eof = Ivar.create () in
              Body.schedule_read body
                ~on_eof:(fun () -> on_close (); Ivar.fill eof ())
                ~on_read:begin fun buf ~off ~len ->
                  let chunk = Bigstring.sub_shared buf ~pos:off ~len in
                  Bigbuffer.add_bigstring buffer chunk ;
                  let data = Bigbuffer.contents buffer in
                  don't_wait_for @@
                  log.log ~media output `OK (lazy (return (Bigstring.to_string chunk)));
                  match media.destruct output data with
                  | Ok body ->
                      Bigbuffer.reset buffer ;
                      on_chunk body ;
                  | Error _msg -> ()
                end ;
              Ivar.read eof >>| fun () -> `Ok (Some ())
        end
      | `Conflict body ->
          handle_error log service body `Conflict (fun v -> `Conflict v)
      | `Error body ->
          handle_error log service body `Internal_server_error (fun v -> `Error v)
      | `Forbidden body ->
          handle_error log service body `Forbidden (fun v -> `Forbidden v)
      | `Not_found body ->
          handle_error log service body `Not_found (fun v -> `Not_found v)
      | `Unauthorized body ->
          handle_error log service body `Unauthorized (fun v -> `Unauthorized v)
      | `Bad_request _
      | `Method_not_allowed _
      | `Unsupported_media_type
      | `Not_acceptable _
      | `Unexpected_status_code _
      | `Connection_failed _
      | `OCaml_exception _ as err -> return err
    end >>= fun ans ->
    return (meth, uri, ans)

  let generic_call meth ?(logger = null_logger) ?headers ?accept ?body_f ?media uri =
    let module Logger = (val logger) in
    begin match body_f with
      | None ->
          Logger.log_empty_request uri
      | Some _ ->
          Logger.log_request Encoding.untyped uri "<stream>"
    end >>= fun log_request ->
    let log = { log = fun ?media -> Logger.log_response log_request ?media } in
    let meth = httpaf_meth_of_meth meth in
    let req = Request.create ?headers meth (Uri.path_and_query uri) in
    internal_call log ?accept ?body_f ?media req

end
