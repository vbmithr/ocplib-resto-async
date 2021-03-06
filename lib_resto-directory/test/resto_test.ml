(**************************************************************************)
(*  ocplib-resto                                                          *)
(*  Copyright (C) 2016, OCamlPro.                                         *)
(*                                                                        *)
(*    All rights reserved.  This file is distributed under the terms      *)
(*    of the GNU Lesser General Public License version 2.1, with the      *)
(*    special exception on linking described in the file LICENSE.         *)
(*                                                                        *)
(**************************************************************************)

open Async
open Services
open Directory
open Resto_directory

open Alcotest

let allowed_methods1 () =
  allowed_methods dir () ["foo";"3";"repeat"] >>= function
  | Ok [`POST] -> Deferred.unit
  | _ -> failf "allowed_methods1"

let allowed_methods2 () =
  allowed_methods dir () ["bar";"3";"4";"add"] >>= function
  | Ok [`GET;`POST] -> Deferred.unit
  | _ -> failf "allowed_methods1"

let basic = Alcotest_async.[
  test_case "allowed_methods1" `Quick allowed_methods1 ;
  test_case "allowed_methods2" `Quick allowed_methods2 ;
]

module Test(Request : sig
    val request:
      ('meth, unit, 'params, 'query, 'input, 'output, 'error) Service.t ->
      'params  -> 'query -> 'input -> [> ('output, 'error) Answer.t ] Deferred.t
  end) = struct

  let one () =
    Request.request describe_service
      ((), ["foo"; "3"]) { recurse = true } () >>= function
    | `Ok dir ->
        Format.printf "@[<v>%a@]@." Resto.Description.pp_print_directory dir ;
        Deferred.unit
    | _ -> fail "one"

  let two () =
    Request.request describe_service
      ((), ["bar"; "3" ; "2." ; "add"]) { recurse = false } () >>= function
    | `Ok dir ->
        Format.printf "@[<v>%a@]@." Resto.Description.pp_print_directory dir ;
        Deferred.unit
    | _ -> fail "two"

  let three () =
    Request.request describe_service ((), []) { recurse = true } () >>= function
    | `Ok dir ->
        Format.printf "@[<v>%a@]@." Resto.Description.pp_print_directory dir ;
        Deferred.unit
    | _ -> fail "three"

  let tests =
    let test service args arg expected () =
      Request.request service args () arg >>| function
      | `Ok v when v = expected -> ()
      | `Ok _
      | #Answer.t -> failwith "test" in
    Alcotest_async.[
      test_case "one" `Quick one ;
      test_case "two" `Quick two ;
      test_case "three" `Quick three ;
      test_case "repeat_service" `Quick (test repeat_service ((), 3) (`A []) (`A (repeat 3 (`A [])))) ;
      test_case "add_service"  `Quick (test add_service ((), 2) 3 5) ;
      test_case "alternate_add_service" `Quick (test alternate_add_service (((), 1), 2.5) () 3.5) ;
      test_case "real_minus_service1" `Quick (test real_minus_service1 (((), 2.5), 1) () 1.5) ;
      test_case "alternate_add_service'" `Quick (test alternate_add_service' (((), 1), 2.) () 3) ;
    ]

end

let split_path path =
  let l = String.length path in
  let rec do_slashes acc i =
    if i >= l then
      List.rev acc
    else if String.get path i = '/' then
      do_slashes acc (i + 1)
    else
      do_component acc i i
  and do_component acc i j =
    if j >= l then
      if i = j then
        List.rev acc
      else
        List.rev (String.sub path i (j - i) :: acc)
    else if String.get path j = '/' then
      do_slashes (String.sub path i (j - i) :: acc) j
    else
      do_component acc i (j + 1) in
  do_slashes [] 0

module Faked = Test(struct
    (** Testing faked client/server communication. *)
    let request (type i) (service: (_,_,_,_,i,_,_) Service.t) params query arg =
      let { Service.meth ; uri ; input } =
        Service.forge_request service params query in
      Format.eprintf "\nREQUEST: %a@." Uri.pp_hum uri ;
      let path = split_path (Uri.path uri) in
      let query =
        List.map
          (fun (n,vs) -> (n, String.concat "," vs))
          (Uri.query uri) in
      let json =
        match input with
        | Service.No_input -> `O []
        | Service.Input input -> Json_encoding.construct input arg in
      lookup dir () meth path >>= function
      | Ok (Service s) -> begin
          let query = Resto.Query.parse s.types.query query in
          begin
            match s.types.input with
            | Service.No_input -> s.handler query ()
            | Service.Input input ->
                s.handler query @@ Json_encoding.destruct input json
          end >>= function
          | `Ok res ->
              let json = Json_encoding.construct s.types.output res in
              return (`Ok (Json_encoding.destruct (Service.output_encoding service) json))
          | _ -> failwith "Unexpected lwt result (1)"
        end
      | _ -> failwith "Unexpected lwt result (2)"
  end)

module Transparent = Test(struct
    let request x = transparent_lookup dir x
  end)

let () =
  Alcotest.run "resto-directory" [
    "basic", basic ;
    "faked", Faked.tests ;
    "transparent", Transparent.tests ;
  ]

