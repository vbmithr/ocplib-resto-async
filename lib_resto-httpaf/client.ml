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

open Async

module Make (Encoding : Resto.ENCODING) = struct

  open Cohttp

  module Media_type = Media_type.Make(Encoding)
  module Service = Resto.MakeService(Encoding)

  type content_type = (string * string)
  type raw_content = [ `write ] Httpaf.Body.t * content_type option
  type content = [ `write ] Httpaf.Body.t * content_type option * Media_type.t option

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
    | `Unexpected_status_code of Cohttp.Code.status_code * content
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
      Cohttp.Code.status_code -> string Deferred.t Lazy.t -> unit Deferred.t
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
      let cpt = ref 0
      type request = int * string
      let log_empty_request uri =
        let id = !cpt in
        let uri = Uri.to_string uri in
        incr cpt ;
        Logs_async.info ~src (fun m -> m ">>>>%d: %s@." id uri) >>= fun () ->
        return (id, uri)
      let log_request ?(media = faked_media) enc uri body =
        let id = !cpt in
        let uri = Uri.to_string uri in
        incr cpt ;
        Logs_async.info ~src begin fun m ->
          m "@[<v 2>>>>>%d: %s@,%a@]@." id uri (media.pp enc) body
        end >>= fun () ->
        return (id, uri)
      let log_response (id, _uri) ?(media = faked_media) enc code body =
        Lazy.force body >>= fun body ->
        Logs_async.info ~src begin fun m -> m "@[<v 2><<<<%d: %s@,%a@]@."
            id (Cohttp.Code.string_of_status code) (media.pp enc) body
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

  let clone_body = function
    | `Pipe s -> `Pipe (Pipe.map ~f:Base.Fn.id s)
    | x -> x

  type log = {
    log:
      'a. ?media:Media_type.t -> 'a Encoding.t -> Cohttp.Code.status_code ->
      string Deferred.t Lazy.t -> unit Deferred.t ;
  }

  let rec call_and_retry_on_502 attempt delay =
    Httpaf_async.Client.request
      ~headers
      (meth :> Code.meth) ~body uri >>= fun (response, ansbody) ->
    let status = Response.status response in
    match status with
    | `Bad_gateway ->
        let log_ansbody = clone_body ansbody in
        log.log ~media:faked_media Encoding.untyped status
          (lazy (Cohttp_async.Body.to_string log_ansbody >>= fun text ->
                 return @@ Format.sprintf
                   "Attempt number %d/10, will retry after %g seconds.\n\
                    Original body follows.\n\
                    %s"
                   attempt (Core_kernel.Time_ns.Span.to_sec delay) text)) >>= fun () ->
        if attempt >= 10 then
          return (response, ansbody)
        else
          let open Core_kernel in
          Clock_ns.after delay >>= fun () ->
          call_and_retry_on_502 (attempt + 1) Time_ns.Span.(delay + of_int_ms 100)
    | _ ->
        return (response, ansbody)

  let internal_call meth (log : log) ?(headers = []) ?accept ?body ?media uri : (content, content) generic_rest_result Deferred.t =
    let headers = List.fold_left (fun headers (header, value) ->
        let header = String.lowercase_ascii header in
        if header <> "host"
        && (String.length header < 2
            || String.sub header 0 2 <> "x-") then
          invalid_arg
            "Resto_cohttp.Client.call: \
             only headers \"host\" or starting with \"x-\" are supported"
        else Header.replace headers header value)
        (Header.init ()) headers in
    let body, headers =
      match body, media with
      | None, _ -> Httpaf.Body.close_writer, headers
      | Some body, None ->
          body, headers
      | Some body, Some media ->
          body, Header.add headers "content-type" (Media_type.name media) in
    let headers =
      match accept with
      | None -> headers
      | Some ranges ->
          Header.add headers "accept" (Media_type.accept_header ranges) in
    Monitor.try_with ~extract_exn:true begin fun () ->
      call_and_retry_on_502 1 Core_kernel.Time_ns.Span.zero >>= fun (response, ansbody) ->
      let headers = Response.headers response in
      let media_name =
        match Header.get headers "content-type" with
        | None -> None
        | Some s ->
            match Utils.split_path s with
            | [x ; y] -> Some (x, y)
            | _      -> None (* ignored invalid *) in
      let media =
        match accept with
        | None -> None
        | Some media_types -> find_media media_name media_types in
      let status = Response.status response in
      match status with
      | `OK -> return (`Ok (Some (ansbody, media_name, media)))
      | `No_content -> return (`Ok None)
      | `Created ->
          (* TODO handle redirection ?? *)
          failwith "Resto_cohttp_client.generic_json_call: unimplemented"
      | `Unauthorized -> return (`Unauthorized (ansbody, media_name, media))
      | `Forbidden -> return (`Forbidden (ansbody, media_name, media))
      | `Not_found -> return (`Not_found (ansbody, media_name, media))
      | `Conflict -> return (`Conflict (ansbody, media_name, media))
      | `Internal_server_error ->
          if media_name = Some ("text", "ocaml.exception") then
            Cohttp_async.Body.to_string ansbody >>= fun msg ->
            return (`OCaml_exception msg)
          else
            return (`Error (ansbody, media_name, media))
      | `Bad_request ->
          Cohttp_async.Body.to_string body >>= fun body ->
          return (`Bad_request body)
      | `Method_not_allowed ->
          let allowed = Cohttp.Header.get_multi headers "accept" in
          return (`Method_not_allowed allowed)
      | `Unsupported_media_type ->
          return  `Unsupported_media_type
      | `Not_acceptable ->
          Cohttp_async.Body.to_string body >>= fun body ->
          return (`Not_acceptable body)
      | code ->
          return
            (`Unexpected_status_code (code, (ansbody, media_name, media)))
    end >>= function
    | Ok v -> return v
    | Error exn -> begin
      let msg =
        match exn with
        | Unix.Unix_error (e, _, _) -> Sexp.to_string (Unix.Error.sexp_of_t e)
        | Failure msg -> msg
        | Invalid_argument msg -> msg
        | e -> Printexc.to_string e in
      return (`Connection_failed msg)
    end

  let handle_error log service (body, media_name, media) status f =
    Cohttp_async.Body.is_empty body >>= fun empty ->
    if empty then
      log.log Encoding.untyped status (lazy (return "")) >>= fun () ->
      return (f None)
    else
      match media with
      | None ->
          return (`Unexpected_error_content_type (body, media_name))
      | Some media ->
          Cohttp_async.Body.to_string body >>= fun body ->
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
          return (Some (Cohttp_async.Body.of_string body),
                      Some media,
                      log_request)
    end >>= fun (body, media, log_request) ->
    let log = { log = fun ?media -> Logger.log_response log_request ?media } in
    return (log, meth, uri, body, media)

  let call_service media_types
      ?logger ?headers ?base service params query body =
    prepare
      media_types ?logger ?base
      service params query body >>= fun (log, meth, uri, body, media) ->
    begin
      internal_call meth log ?headers ~accept:media_types ?body ?media uri >>= function
      | `Ok None ->
          log.log Encoding.untyped `No_content (lazy (return "")) >>= fun () ->
          return (`Ok None)
      | `Ok (Some (body, media_name, media)) -> begin
          match media with
          | None ->
              return (`Unexpected_content_type (body, media_name))
          | Some media ->
              Cohttp_async.Body.to_string body >>= fun body ->
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
      service params query body >>= fun (log, meth, uri, body, media) ->
    begin
      internal_call meth log ?headers ~accept:media_types ?body ?media uri >>= function
      | `Ok None ->
          on_close () ;
          log.log Encoding.untyped `No_content (lazy (return "")) >>= fun () ->
          return (`Ok None)
      | `Ok (Some (body, media_name, media)) -> begin
          match media with
          | None ->
              return (`Unexpected_content_type (body, media_name))
          | Some media ->
              let stream = Cohttp_async.Body.to_pipe body in
              Pipe.read stream >>= function
              | `Eof ->
                  on_close () ;
                  return (`Ok None)
              | `Ok chunk ->
                  let buffer = Buffer.create 2048 in
                  let output = Service.output_encoding service in
                  let rec loop = function
                    | `Eof -> on_close () ; Deferred.unit
                    | `Ok chunk ->
                        Buffer.add_string buffer chunk ;
                        let data = Buffer.contents buffer in
                        log.log ~media output
                          `OK (lazy (return chunk)) >>= fun () ->
                        match media.destruct output data with
                        | Ok body ->
                            Buffer.reset buffer ;
                            on_chunk body ;
                            Pipe.read stream >>= loop
                        | Error _msg ->
                            Pipe.read stream >>= loop in
                  don't_wait_for (loop (`Ok chunk)) ;
                  return (`Ok (Some (fun () ->
                      don't_wait_for (Pipe.drain stream) ;
                      ())))
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

  let generic_call meth ?(logger = null_logger) ?headers ?accept ?body ?media uri =
    let module Logger = (val logger) in
    begin match body with
      | None->
          Logger.log_empty_request uri
      | Some (`Pipe _) ->
          Logger.log_request Encoding.untyped uri "<stream>"
      | Some body ->
          Cohttp_async.Body.to_string body >>= fun body ->
          Logger.log_request ?media Encoding.untyped uri body
    end >>= fun log_request ->
    let log = { log = fun ?media -> Logger.log_response log_request ?media } in
    internal_call meth log ?headers ?accept ?body ?media uri

end