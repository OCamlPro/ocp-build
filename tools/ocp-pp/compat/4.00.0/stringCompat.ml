(**************************************************************************)
(*                                                                        *)
(*   Typerex Tools                                                        *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU General Public License version 3 described in the file       *)
(*   LICENSE.                                                             *)
(*                                                                        *)
(**************************************************************************)

type bytes = string

module Bytes = struct
  include String
  let to_string t = String.copy t
  let of_string t = String.copy t
  let unsafe_to_string t = t
  let unsafe_of_string t = t
  let sub_string = String.sub
  let blit_string = String.blit
 end

module Buffer = struct
  include Buffer
  let to_bytes b = contents b
  let add_subbytes = add_substring
end

module Marshal = struct
  include Marshal
  let from_bytes = from_string
end

let print_bytes = print_string
let prerr_bytes = prerr_string
let output_bytes = output_string
let output_substring = output
let really_input_string ic len =
  let s = String.create len in
  really_input ic s 0 len;
  s

module StringSet = Set.Make(String)

module StringMap = struct
  module M = Map.Make(String)
  include M
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
end
