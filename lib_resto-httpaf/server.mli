(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

(** Typed RPC services: server implementation. *)

open Async

module Make (Encoding : Resto.ENCODING) (Log : Logs_async.LOG) : sig
  module Directory : module type of Resto_directory.Make(Encoding)
  module Media_type : module type of Media_type.Make(Encoding)
  val launch :
    ?max_connections:int ->
    ?max_accepts_per_batch:int ->
    ?backlog:int ->
    ?socket:([ `Unconnected ], Socket.Address.Inet.t) Socket.t ->
    ?config:Httpaf.Config.t ->
    ?cors:Cors.t ->
    media_types:Media_type.t list ->
    unit Directory.t ->
    (Socket.Address.Inet.t, 'listening_on) Tcp.Where_to_listen.t ->
    (Socket.Address.Inet.t, 'listening_on) Tcp.Server.t Deferred.t
end
