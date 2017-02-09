(* SUPERSEDED BY StringCompat
(**************************************************************************)
(*                                                                        *)
(*   Typerex Libraries                                                    *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Map = Map.Make(String)

include Map

let of_list list =
  let map = ref empty in
  List.iter (fun (x,y) -> map := add x y !map) list;
  !map

let to_list map =
  let list = ref [] in
  iter (fun x y -> list := (x,y) :: !list) map;
  List.rev !list

let to_list_of_keys map =
  let list = ref [] in
  iter (fun x y -> list := x :: !list) map;
  List.rev !list
*)
