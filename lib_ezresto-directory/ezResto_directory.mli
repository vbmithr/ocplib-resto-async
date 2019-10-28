(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

open Async_kernel
open Httpaf
open EzResto

module Answer : module type of Resto_directory.Answer

(** Possible error while registring services. *)
type step =
  | Static of string
  | Dynamic of Arg.descr
  | DynamicTail of Arg.descr

type conflict =
  | CService of Method.t | CDir | CBuilder | CTail
  | CTypes of Arg.descr * Arg.descr
  | CType of Arg.descr * string list

exception Conflict of step list * conflict

(** Dispatch tree *)
type directory

(** Empty tree *)
val empty: directory

val prefix: 'a Path.t -> directory -> directory
val merge: directory -> directory -> directory

type 'input input =
  | No_input : unit input
  | Input : 'input Json_encoding.encoding -> 'input input

type ('q, 'i, 'o, 'e) types = {
  query : 'q Resto.Query.t ;
  input : 'i input ;
  output : 'o Json_encoding.encoding ;
  error : 'e Json_encoding.encoding ;
}

type registered_service =
  | Service :
      { types : ('q, 'i, 'o, 'e) types ;
        handler : ('q -> 'i -> ('o, 'e) Answer.t Deferred.t) ;
      } -> registered_service

type lookup_error =
  [ `Not_found (* 404 *)
  | `Method_not_allowed of Method.t list (* 405 *)
  | `Cannot_parse_path of string list * Arg.descr * string (* 400 *)
  ]

(** Resolve a service. *)
val lookup: directory -> Method.t -> string list -> (registered_service, [> lookup_error ]) result Deferred.t

val allowed_methods:
  directory -> string list ->
  (Method.t list, [> lookup_error ]) result Deferred.t

val transparent_lookup:
  directory ->
  ('meth, 'params, 'query, 'input, 'output, 'error) EzResto.service ->
  'params -> 'query -> 'input -> [> ('output, 'error) Answer.t ] Deferred.t


(** Registring handler in service tree. *)
val register:
  directory ->
  ('meth, 'params, 'query, 'input, 'output, 'error) EzResto.service ->
  ('params -> 'query -> 'input -> ('output, 'error) Answer.t Deferred.t) ->
  directory

(** Registring handler in service tree. Curryfied variant.  *)
val register0:
  directory ->
  ('meth, unit, 'q, 'i, 'o, 'e) EzResto.service ->
  ('q -> 'i -> ('o, 'e) Answer.t Deferred.t) ->
  directory

val register1:
  directory ->
  ('meth, unit * 'a, 'q, 'i, 'o, 'e) EzResto.service ->
  ('a -> 'q -> 'i -> ('o, 'e) Answer.t Deferred.t) ->
  directory

val register2:
  directory ->
  ('meth, (unit * 'a) * 'b, 'q, 'i, 'o, 'e) EzResto.service ->
  ('a -> 'b -> 'q -> 'i -> ('o, 'e) Answer.t Deferred.t) ->
  directory

val register3:
  directory ->
  ('meth, ((unit * 'a) * 'b) * 'c, 'q, 'i, 'o, 'e) EzResto.service ->
  ('a -> 'b -> 'c -> 'q -> 'i -> ('o, 'e) Answer.t Deferred.t) ->
  directory

val register4:
  directory ->
  ('meth, (((unit * 'a) * 'b) * 'c) * 'd, 'q, 'i, 'o, 'e) EzResto.service ->
  ('a -> 'b -> 'c -> 'd -> 'q -> 'i -> ('o, 'e) Answer.t Deferred.t) ->
  directory

val register5:
  directory ->
  ('meth, ((((unit * 'a) * 'b) * 'c) * 'd) * 'e, 'q, 'i, 'o, 'e) EzResto.service ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'q -> 'i -> ('o, 'e) Answer.t Deferred.t) ->
  directory

(** Registring dynamic subtree. *)
val register_dynamic_directory:
  ?descr:string ->
  directory ->
  'params Path.t ->
  ('params -> directory Deferred.t) ->
  directory

(** Registring dynamic subtree. (Curryfied variant) *)
val register_dynamic_directory1:
  ?descr:string ->
  directory ->
  (unit * 'a) Path.t ->
  ('a -> directory Deferred.t) ->
  directory

val register_dynamic_directory2:
  ?descr:string ->
  directory ->
  ((unit * 'a) * 'b) Path.t ->
  ('a -> 'b -> directory Deferred.t) ->
  directory

val register_dynamic_directory3:
  ?descr:string ->
  directory ->
  (((unit * 'a) * 'b) * 'c) Path.t ->
  ('a -> 'b -> 'c -> directory Deferred.t) ->
  directory

(** Registring a description service. *)
val register_describe_directory_service:
  directory -> EzResto.description_service -> directory

