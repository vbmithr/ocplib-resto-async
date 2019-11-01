open Core
open Async

open Resto_httpaf

let log = Logs.Src.create "resto-httpaf.test"
module Log = (val Logs_async.src_log log : Logs_async.LOG)

module Encoding = struct
  include Json_encoding

  let conv ffrom fto a = conv ffrom fto ~schema:(schema a) a
  type 'a t = 'a encoding
  let untyped = obj1 (req "untyped" string)
  type schema = Json_schema.schema

  let description_request_encoding =
    conv
      (fun { Resto.Description.recurse } -> recurse)
      (fun recurse -> { recurse })
      (obj1 (dft "recursive" bool false))

  let description_answer_encoding =
    conv
      (function Resto.Description.Empty -> () | _ -> assert false)
      (fun () -> Empty)
      unit
end

module MediaType = struct
  include Resto_httpaf.Media_type.Make(Encoding)

  let json  = {
    name = Cohttp.Accept.MediaType ("application", "json") ;
    q = Some 1000 ;
    pp = begin fun _enc ppf raw ->
      match Result.try_with begin fun () ->
          Ezjsonm.value_from_string raw
        end with
      | Error err ->
          Format.fprintf ppf
            "@[Invalid JSON:@ \
            \ - @[<v 2>Error:@ %a@]\
            \ - @[<v 2>Raw data:@ %s@]@]"
            Exn.pp err raw
      | Ok json ->
          Format.fprintf ppf "%s" (Ezjsonm.value_to_string json)
    end ;
    construct = begin fun enc v ->
      Ezjsonm.value_to_string ~minify:true @@
      Json_encoding.construct enc v
    end ;
    destruct = begin fun enc body ->
      match Result.try_with begin fun () ->
          Ezjsonm.value_from_string body
        end with
      | Error e  -> Error (Exn.to_string e)
      | Ok json ->
          try Ok (Json_encoding.destruct enc json)
          with Json_encoding.Cannot_destruct (_, exn) ->
            Error (Format.asprintf "%a"
                     (fun fmt -> Json_encoding.print_error fmt)
                     exn)
    end ;
  }
end

module Directory = Resto_directory.Make(Encoding)
module Srv = Server.Make(Encoding)(Log)

let main port =
  Srv.launch ~media_types:[MediaType.json] Directory.empty
    (Tcp.Where_to_listen.of_port port) >>= fun _srv ->
  Log.debug
    (fun m -> m "Server launched on port %d" port) >>= fun () ->
  Deferred.never () >>= fun () ->
  Deferred.Or_error.ok_unit

let command =
  Command.async_or_error ~summary:"Test REST server" begin
    let open Command.Let_syntax in
    [%map_open
      let port =
        flag_optional_with_default_doc "port"
          int sexp_of_int ~default:5573 ~doc:"int TCP port to use"
      and () =
        Logs_async_reporter.set_level_via_param [] in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main port
    ]
  end

let () = Command.run command
