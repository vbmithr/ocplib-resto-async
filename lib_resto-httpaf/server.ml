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

let pp_print_addr ppf a =
  Format.fprintf ppf "%s" (Socket.Address.to_string a)

module AddressMap = Map.Make(Socket.Address.Inet.Blocking_sexp)

module Make (Encoding : Resto.ENCODING) (Log : Logs_async.LOG) = struct

  open Httpaf
  module Service = Resto.MakeService(Encoding)
  module Directory = Resto_directory.Make(Encoding)

  module Media_type = Media_type.Make(Encoding)

  type state = {
    root : unit Directory.directory ;
    mutable streams : string Pipe.Reader.t AddressMap.t ;
    cors : Cors.t ;
    media_types : Media_type.t list ;
    default_media_type : string * Media_type.t ;
  }

  let create_pipe state addr to_string s =
    let stream = Pipe.map s ~f:to_string in
    state.streams <- AddressMap.set ~key:addr ~data:stream state.streams ;
    stream

  let get_input_media_type server { Request.headers; _ } =
    match Headers.get headers "content-type" with
    | None -> Deferred.Result.return (snd server.default_media_type)
    | Some content_type ->
        match Utils.split_path content_type with
        | [x ; y] -> begin
            match Media_type.find_media (x, y) server.media_types with
            | None ->
                return (Error (`Unsupported_media_type content_type))
            | Some media_type ->
                Deferred.Result.return media_type
          end
        | _ ->
            return (Error (`Unsupported_media_type content_type))

  let get_accept server { Request.headers; _ } =
    match Headers.get headers "accept" with
    | None -> Deferred.Result.return server.default_media_type
    | Some accepted ->
        match Media_type.resolve_accept_header
                server.media_types (Some accepted) with
        | None -> return (Error `Not_acceptable)
        | Some media_type -> Deferred.Result.return media_type

  let get_path reqd addr =
    let req = Reqd.request reqd in
    (* FIXME: check inbound adress *)
    let uri = Uri.of_string req.target in
    let path = Uri.pct_decode (Uri.path uri) in
    Log.info begin fun m ->
      m "(%a) received request to %s" pp_print_addr addr path
    end >>| fun () ->
    Utils.split_path path

  let handle_most state addr reqd =
    let req = Reqd.request reqd in
    get_path reqd addr >>= fun path ->
    Directory.lookup state.root () req.meth path >>=? fun (Directory.Service s) ->
    get_input_media_type state req >>=? fun input_media_type ->
    Log.debug (fun m -> m "(%a) input media type %s" pp_print_addr
                  addr (Media_type.name input_media_type)) >>= fun () ->
    get_accept state req >>=? fun (output_content_type, output_media_type) ->
    let query = Uri.(query (of_string req.target)) in
    begin match Resto.Query.parse s.types.query
                  (List.Assoc.map query ~f:(String.concat ~sep:",")) with
    | exception (Resto.Query.Invalid s) -> return (Error (`Cannot_parse_query s))
    | query -> Deferred.Result.return query
    end >>=? fun query ->
    Log.debug (fun m -> m "(%a) ouput media type %s" pp_print_addr
                  addr (Media_type.name output_media_type)) >>= fun () ->
    let output = output_media_type.construct s.types.output in
    let error = output_media_type.construct s.types.error in
    let headers =
      Headers.(add empty "content-type" output_content_type) in
    let headers =
      Cors.add_allow_origin headers state.cors (Headers.get headers "origin") in
    begin
      match s.types.input with
      | Service.No_input -> Deferred.ok (s.handler query ())
      | Service.Input input ->
          Utils.read_body (Reqd.request_body reqd) >>= fun body ->
          match input_media_type.destruct input body with
          | Error s -> return (Error (`Cannot_parse_body s))
          | Ok body -> Deferred.ok (s.handler query body)
    end >>|? function
    | `Ok o ->
        Reqd.respond_with_string reqd (Response.create ~headers `OK) (output o)
    | `OkPipe o ->
        let body = create_pipe state addr output o in
        let b = Reqd.respond_with_streaming reqd (Response.create ~headers `OK) in
        don't_wait_for (Pipe.iter_without_pushback body ~f:(Body.write_string b))
    | `Created loc ->
        let headers =
          Option.value_map loc ~default:headers ~f:(Headers.add headers "location") in
        Reqd.respond_with_string reqd (Response.create ~headers `Created) ""
    | `No_content ->
        Reqd.respond_with_string reqd (Response.create `No_content) ""
    | `Unauthorized (Some e) ->
        Reqd.respond_with_string
          reqd (Response.create ~headers `Unauthorized) (error e)
    | `Forbidden (Some e) ->
        Reqd.respond_with_string
          reqd (Response.create ~headers `Forbidden) (error e)
    | `Not_found (Some e) ->
        Reqd.respond_with_string
          reqd (Response.create ~headers `Not_found) (error e)
    | `Conflict (Some e) ->
        Reqd.respond_with_string
          reqd (Response.create ~headers `Conflict) (error e)
    | `Error (Some e) ->
        Reqd.respond_with_string
          reqd (Response.create ~headers `Internal_server_error) (error e)
    | _ -> assert false

  let handle_options state addr reqd =
    let { Request.headers; _ } = Reqd.request reqd in
    get_path reqd addr >>= fun path ->
    let origin_header = Headers.get headers "origin" in
    begin
      (* Default OPTIONS handler for CORS preflight *)
      if origin_header = None then
        Directory.allowed_methods state.root () path
      else
        match Headers.get headers
                "Access-Control-Request-Method" with
        | None ->
            Directory.allowed_methods state.root () path
        | Some meth ->
            let meth = Method.of_string meth in
            Directory.lookup state.root () meth path >>=? fun _handler ->
            return (Ok [ meth ])
    end >>=? fun cors_allowed_meths ->
    Log.info begin fun m ->
      m "(%a) RPC preflight" pp_print_addr addr
    end >>= fun () ->
    let headers = Headers.add_multi headers
        ["Access-Control-Allow-Methods",
         List.map ~f:Method.to_string cors_allowed_meths] in
    let headers = Cors.add_headers headers state.cors origin_header in
    Deferred.Result.return (Reqd.respond_with_string reqd (Response.create ~headers `OK) "")

  let respond_close ?(headers=Headers.empty) reqd status fmt =
    let headers = Headers.(add_list headers ["Content-Type", "text/plain";
                                             "Connection", "close"]) in
    Format.ksprintf
      (Reqd.respond_with_string reqd
         (Response.create ~headers status)) fmt

  let request_handler_exn state addr reqd =
    let req = Reqd.request reqd in
    begin match req.meth with
      | `OPTIONS -> handle_options state addr reqd
      | `DELETE | `GET | `POST | `PUT | `Other "PATCH" ->
          handle_most state addr reqd
      | `HEAD -> return (Error `Not_implemented)
      | _ -> return (Error `Not_implemented)
    end >>| function
    | Ok () -> ()
    | Error `Not_implemented ->
        respond_close reqd `Not_implemented ""
    | Error `Method_not_allowed methods ->
        let headers =
          Headers.(add_multi empty ["allow", List.map ~f:Method.to_string methods]) in
        respond_close ~headers reqd `Method_not_allowed ""
    | Error `Cannot_parse_path (context, arg, value) ->
        respond_close reqd `Bad_request
          "Failed to parsed an argument in path. After \"%s\", \
           the value \"%s\" is not acceptable for type \"%s\""
          (String.concat ~sep:"/" context) value arg.Resto.Arg.name
    | Error `Cannot_parse_body s ->
        respond_close
          reqd `Bad_request "Failed to parse the request body: %s" s
    | Error `Cannot_parse_query s ->
        respond_close
          reqd `Bad_request "Failed to parse the query string: %s" s
    | Error `Not_acceptable ->
        respond_close reqd `Not_acceptable
          "%s" (Media_type.acceptable_encoding state.media_types)
    | Error `Unsupported_media_type _ ->
        respond_close reqd `Unsupported_media_type ""
    | Error `Not_found ->
        respond_close reqd `Not_found ""

  let request_handler state addr reqd =
    don't_wait_for begin
      Monitor.try_with ~extract_exn:true begin fun () ->
        request_handler_exn state addr reqd
      end >>= function
      | Ok r -> return r
      | Error exn ->
          Reqd.report_exn reqd exn ;
          Deferred.unit
    end

  let error_handler _addr ?request:_ _err _f =
    (* | Error Not_found ->
       *     let status = `Not_found in
       *     let body = Cohttp_async.Body.empty in
       *     return (Response.make ~status (), body)
       * | Error exn ->
       *     let headers = Header.init () in
       *     let headers =
       *       Header.add headers "content-type" "text/ocaml.exception" in
       *     let status = `Internal_server_error in
       *     let body = Cohttp_async.Body.of_string (Printexc.to_string exn) in
       *     return (Response.make ~status ~headers (), body) *)
    assert false

  let launch
      ?max_connections ?max_accepts_per_batch
      ?backlog ?socket ?config
      ?(cors = Cors.default)
      ~media_types
      root listen_on =
    let default_media_type =
      match Media_type.first_complete_media media_types with
      | None -> invalid_arg "RestoHttpaf.launch(empty media type list)"
      | Some ((l, r), m) -> l^"/"^r, m in
    let t = {
      root ;
      streams = AddressMap.empty ;
      cors ;
      media_types ;
      default_media_type ;
    } in
    let on_handler_error addr exn =
      don't_wait_for begin
        Log.info begin fun m ->
          m "(%a) connection closed (%a)" pp_print_addr addr Exn.pp exn
        end >>| fun () ->
        Option.iter ~f:Pipe.close_read (AddressMap.find t.streams addr)
      end
      (* and on_exn = function
       *   | Unix.Unix_error (Unix.EADDRINUSE, "bind", _) ->
       *       log_error "RPC server port already taken, \
       *                  the node will be shutdown" ;
       *       exit 1
       *   | Unix.Unix_error (ECONNRESET, _, _)
       *   | Unix.Unix_error (EPIPE, _, _)  -> ()
       *   | exn -> !Lwt.async_exception_hook exn *)
    in
    Tcp.Server.create_sock
      ?max_connections ?max_accepts_per_batch ?backlog ?socket
      ~on_handler_error:(`Call on_handler_error) listen_on begin fun addr s ->
      Httpaf_async.Server.create_connection_handler
        ?config
        ~request_handler:(request_handler t)
        ~error_handler addr s
    end

end
