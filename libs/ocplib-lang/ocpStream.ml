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

(* {{extend Stream}} *)
include Stream

let to_list stream =
  let list = ref [] in
  Stream.iter (
    fun token ->
      list := token :: !list
  ) stream;
  List.rev !list

(* Build a stream from a list of lines *)
let of_lines lines =
  let lines = Array.of_list lines in
  let fn = OcpString.indexes lines in
  let elt i =
    try let n,j = fn i in Some lines.(n).[j]
    with OcpString.Out_of_bounds -> None in
  from elt

let is_empty s =
  try ignore (Stream.peek s); false
  with Stream.Failure -> true
