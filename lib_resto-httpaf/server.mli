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

  (** A handle on the server worker. *)
  type ('a, 'listening_on) server
    constraint 'a = [< Async_unix.Socket.Address.t ]

  (** Promise a running RPC server.*)
  val launch :
    ?max_connections:int ->
    ?max_accepts_per_batch:int ->
    ?backlog:int ->
    ?socket:([ `Unconnected ], [< Socket.Address.t ] as 'b) Socket.t ->
    ?cors:Cors.t ->
    media_types:Media_type.Make(Encoding).t list ->
    unit Resto_directory.Make(Encoding).t ->
    (Socket.Address.t, 'listening_on) Tcp.Where_to_listen.t ->
    (Socket.Address.t, 'listening_on) server Deferred.t
end
