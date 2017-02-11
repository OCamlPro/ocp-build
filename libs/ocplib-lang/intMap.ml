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

module Map = Map.Make(struct type t = int
                             let compare (x:int) y = compare x y
end)

include Map

let to_list map =
  let list = ref [] in
  iter (fun x y -> list := (x,y) :: !list) map;
  List.rev !list

let to_list1 map =
  let list = ref [] in
  iter (fun x _y -> list := x :: !list) map;
  List.rev !list

let to_list2 map =
  let list = ref [] in
  iter (fun _x y -> list := y :: !list) map;
  List.rev !list

exception MinElt
let exn_MinElt = MinElt

let min_elt map =
  let x = ref None in
  try
    iter (fun key v -> x := Some (key, v); raise exn_MinElt) map;
    None
  with MinElt -> !x
