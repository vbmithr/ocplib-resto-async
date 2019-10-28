(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

(** Typed RPC services: client implementation. *)

open Async

module Make (Encoding : Resto.ENCODING) : sig

  open Httpaf

  module Service : (module type of (struct include Resto.MakeService(Encoding) end))

  type content_type = (string * string)
  type raw_content = [ `read ] Body.t * content_type option
  type content = [ `read ] Body.t * content_type option * Media_type.Make(Encoding).t option

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

  module type LOGGER = sig
    type request
    val log_empty_request: Uri.t -> request Deferred.t
    val log_request:
      ?media:Media_type.Make(Encoding).t -> 'a Encoding.t ->
      Uri.t -> string -> request Deferred.t
    val log_response:
      request -> ?media:Media_type.Make(Encoding).t -> 'a Encoding.t ->
      Status.t -> string Deferred.t Lazy.t -> unit Deferred.t
  end

  type logger = (module LOGGER)

  val null_logger: logger
  val timings_logger: Logs.src -> logger
  val full_logger: Logs.src -> logger

  val generic_call:
    Method.t ->
    ?logger:logger ->
    ?headers:Headers.t ->
    ?accept:Media_type.Make(Encoding).t list ->
    ?body_f:([ `write ] Body.t -> unit) ->
    ?media:Media_type.Make(Encoding).t ->
    Uri.t -> (content, content) generic_rest_result Deferred.t

  type ('o, 'e) service_result =
    [ ('o, 'e option) generic_rest_result
    | `Unexpected_content_type of raw_content
    | `Unexpected_content of (string * Media_type.Make(Encoding).t) * string
    | `Unexpected_error_content_type of raw_content
    | `Unexpected_error_content of (string * Media_type.Make(Encoding).t) * string ]

  val call_service:
    Media_type.Make(Encoding).t list ->
    ?logger:logger ->
    ?headers:Headers.t ->
    ?base:Uri.t ->
    ([< Method.t ], unit, 'p, 'q, 'i, 'o, 'e) Service.t ->
    'p -> 'q -> 'i -> (Method.t * Uri.t * ('o, 'e) service_result) Deferred.t

  val call_streamed_service:
    Media_type.Make(Encoding).t list ->
    ?logger:logger ->
    ?headers:Headers.t ->
    ?base:Uri.t ->
    ([< Method.t ], unit, 'p, 'q, 'i, 'o, 'e) Service.t ->
    on_chunk: ('o -> unit) ->
    on_close: (unit -> unit) ->
    'p -> 'q -> 'i ->
    (Method.t * Uri.t * (unit, 'e) service_result) Deferred.t

end
