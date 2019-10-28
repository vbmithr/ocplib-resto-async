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

  type ('a, 'listening_on) server = {
    root : unit Directory.directory ;
    mutable streams : string Pipe.Reader.t AddressMap.t ;
    cors : Cors.t ;
    media_types : Media_type.t list ;
    default_media_type : string * Media_type.t ;
    mutable server : ('a, 'listening_on) Tcp.Server.t Deferred.t ;
  }

  let create_pipe server addr to_string s =
    let stream = Pipe.map s ~f:to_string in
    server.streams <- AddressMap.set ~key:addr ~data:stream server.streams ;
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

  let handle_most ({ Request.meth; target; headers; _ } as req) path server addr =
    Directory.lookup server.root () meth path >>=? fun (Directory.Service s) ->
    get_input_media_type server req >>=? fun input_media_type ->
    Log.debug (fun m -> m "(%a) input media type %s" pp_print_addr
                  addr (Media_type.name input_media_type)) >>= fun () ->
    get_accept server req >>=? fun (output_content_type, output_media_type) ->
    let query = Uri.(query (of_string target)) in
    begin match Resto.Query.parse s.types.query
                  (List.Assoc.map query ~f:(String.concat ~sep:",")) with
    | exception (Resto.Query.Invalid s) -> return (Error (`Cannot_parse_query s))
    | query -> Deferred.Result.return query
    end >>=? fun query ->
    Log.debug (fun m -> m "(%a) ouput media type %s" pp_print_addr
                  addr (Media_type.name output_media_type)) >>= fun () ->
    let output = output_media_type.construct s.types.output
    and error = function
    | None -> assert false
    | Some e ->
        let s = output_media_type.construct s.types.error e in
        Cohttp_async.Body.of_string s,
        Transfer.Fixed (Int64.of_int (String.length s)) in
    let new_headers =
      Headers.(add empty "content-type" output_content_type) in
    let new_headers = Cors.add_allow_origin
        headers server.cors (Headers.get headers "origin") in
    begin
      match s.types.input with
      | Service.No_input -> Deferred.ok (s.handler query ())
      | Service.Input input ->
          Utils.read_body body >>= fun body ->
          match input_media_type.destruct input body with
          | Error s -> return (Error (`Cannot_parse_body s))
          | Ok body -> Deferred.ok (s.handler query body)
    end >>=? function
    | `Ok o ->
        let body = output o in
        let encoding =
          Transfer.Fixed (Int64.of_int (String.length body)) in
        Deferred.Result.return
          (Response.make ~status:`OK ~encoding ~headers (),
           Cohttp_async.Body.of_string body)
    | `OkPipe o ->
        let body = create_pipe server addr output o in
        let encoding = Transfer.Chunked in
        return (Ok
                  (Response.make ~status:`OK ~encoding ~headers (),
                   Cohttp_async.Body.of_pipe body))
    | `Created s ->
        let headers = Header.init () in
        let headers =
          match s with
          | None -> headers
          | Some s -> Header.add headers "location" s in
        Deferred.Result.return
          (Response.make ~status:`Created  ~headers (),
           Cohttp_async.Body.empty)
    | `No_content ->
        Deferred.Result.return
          (Response.make ~status:`No_content (),
           Cohttp_async.Body.empty)
    | `Unauthorized e ->
        let body, encoding = error e in
        let status = `Unauthorized in
        Deferred.Result.return
          (Response.make ~status ~encoding ~headers (), body)
    | `Forbidden e ->
        let body, encoding = error e in
        let status = `Forbidden in
        Deferred.Result.return
          (Response.make ~status ~encoding ~headers (), body)
    | `Not_found e ->
        let body, encoding = error e in
        let status = `Not_found in
        Deferred.Result.return
          (Response.make ~status ~encoding ~headers (), body)
    | `Conflict e ->
        let body, encoding = error e in
        let status = `Conflict in
        Deferred.Result.return
          (Response.make ~status ~encoding ~headers (), body)
    | `Error e ->
        let body, encoding = error e in
        let status = `Internal_server_error in
        Deferred.Result.return
          (Response.make ~status ~encoding ~headers (), body)

  let handle_options server { Request.headers; _ } path =
    let origin_header = Headers.get headers "origin" in
    begin
      (* Default OPTIONS handler for CORS preflight *)
      if origin_header = None then
        Directory.allowed_methods server.root () path
      else
        match Headers.get headers
                "Access-Control-Request-Method" with
        | None ->
            Directory.allowed_methods server.root () path
        | Some meth ->
            let meth = Method.of_string meth in
            Directory.lookup server.root () meth path >>=? fun _handler ->
            return (Ok [ meth ])
        | _ ->
            return (Error `Not_found)
    end >>=? fun cors_allowed_meths ->
    Log.info begin fun m ->
      m "(%a) RPC preflight" pp_print_addr addr
    end >>= fun () ->
    let headers = Header.init () in
    let headers =
      Header.add_multi headers
        "Access-Control-Allow-Methods"
        (List.map Resto.string_of_meth cors_allowed_meths) in
    let headers = Cors.add_headers headers server.cors origin_header in
    return (Ok
              (Response.make ~flush:true ~status:`OK ~headers (),
               Cohttp_async.Body.empty))

  let request_handler addr reqd =
    let req = Reqd.request reqd in
    (* FIXME: check inbound adress *)
    let uri = Uri.of_string target in
    let path = Uri.pct_decode (Uri.path uri) in
    Log.info begin fun m ->
      m "(%a) receive request to %s" pp_print_addr addr path
    end >>= fun () ->
    let path = Utils.split_path path in
    begin match req.Request.meth with
      | `DELETE | `GET | `POST | `PUT | `Other "PATCH" -> handle_most path server
      | `OPTIONS -> handle_options path
      | `HEAD -> return (Error `Not_implemented)
      | _ -> return (Error `Not_implemented)
    end >>= function
    | Ok answer -> return answer
    | Error `Not_implemented ->
        return
          (Response.make ~status:`Not_implemented (),
           Cohttp_async.Body.empty)
    | Error `Method_not_allowed methods ->
        let headers = Header.init () in
        let headers =
          Header.add_multi headers "allow"
            (List.map Resto.string_of_meth methods) in
        return
          (Response.make ~status:`Method_not_allowed ~headers (),
           Cohttp_async.Body.empty)
    | Error `Cannot_parse_path (context, arg, value) ->
        let headers = Header.init () in
        let headers =
          Header.add headers "content-type" "text/plain" in
        return
          (Response.make ~status:`Bad_request ~headers (),
           Format.kasprintf Cohttp_async.Body.of_string
             "Failed to parsed an argument in path. After \"%s\", \
              the value \"%s\" is not acceptable for type \"%s\""
             (String.concat "/" context) value arg.name)
    | Error `Cannot_parse_body s ->
        let headers = Header.init () in
        let headers =
          Header.add headers "content-type" "text/plain" in
        return
          (Response.make ~status:`Bad_request ~headers (),
           Format.kasprintf Cohttp_async.Body.of_string
             "Failed to parse the request body: %s" s)
    | Error `Cannot_parse_query s ->
        let headers = Header.init () in
        let headers =
          Header.add headers "content-type" "text/plain" in
        return
          (Response.make ~status:`Bad_request ~headers (),
           Format.kasprintf Cohttp_async.Body.of_string
             "Failed to parse the query string: %s" s)
    | Error `Not_acceptable ->
        let accepted_encoding =
          Media_type.acceptable_encoding server.media_types in
        return
          (Response.make ~status:`Not_acceptable (),
           Cohttp_async.Body.of_string accepted_encoding)
    | Error `Unsupported_media_type _ ->
        return
          (Response.make ~status:`Unsupported_media_type (),
           Cohttp_async.Body.empty)
    | Error `Not_found ->
        return
          (Response.make ~status:`Not_found (),
           Cohttp_async.Body.empty)

  (* Promise a running RPC server. *)

  let launch
      ?max_connections ?max_accepts_per_batch
      ?backlog ?socket ?config
      ?(cors = Cors.default)
      ~media_types
      root listen_on =
    let default_media_type =
      match Media_type.first_complete_media media_types with
      | None -> invalid_arg "RestoCohttp.launch(empty media type list)"
      | Some ((l, r), m) -> l^"/"^r, m in
    let serv_ivar = Ivar.create () in
    let t = {
      root ;
      streams = AddressMap.empty ;
      cors ;
      media_types ;
      default_media_type ;
      server = Ivar.read serv_ivar ;
    } in
    let on_handler_error addr _exn =
      don't_wait_for @@ begin
        Log.info begin fun m ->
          m "connection closed %a" pp_print_addr addr
        end >>| fun () ->
        Base.Option.iter ~f:Pipe.close_read
          (AddressMap.find_opt addr t.streams)
      end
      (* and on_exn = function
       *   | Unix.Unix_error (Unix.EADDRINUSE, "bind", _) ->
       *       log_error "RPC server port already taken, \
       *                  the node will be shutdown" ;
       *       exit 1
       *   | Unix.Unix_error (ECONNRESET, _, _)
       *   | Unix.Unix_error (EPIPE, _, _)  -> ()
       *   | exn -> !Lwt.async_exception_hook exn
       * and callback ~body socket req =
       *   Monitor.try_with ~extract_exn:true
       *     (fun () -> callback t socket req body) >>= function
       *   | Ok r -> return r
       *   | Error Not_found ->
       *       let status = `Not_found in
       *       let body = Cohttp_async.Body.empty in
       *       return (Response.make ~status (), body)
       *   | Error exn ->
       *       let headers = Header.init () in
       *       let headers =
       *         Header.add headers "content-type" "text/ocaml.exception" in
       *       let status = `Internal_server_error in
       *       let body = Cohttp_async.Body.of_string (Printexc.to_string exn) in
       *       return (Response.make ~status ~headers (), body) *)
    in
    let error_handler _ ?request:_ _ _ = () in
    Tcp.Server.create_sock
      ?max_connections ?max_accepts_per_batch ?backlog ?socket
      ~on_handler_error:(`Call on_handler_error)
      listen_on begin fun addr s ->
      Httpaf_async.Server.create_connection_handler
        ?config ~request_handler ~error_handler addr s
    end

end
