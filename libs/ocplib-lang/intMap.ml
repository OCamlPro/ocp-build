(**************************************************************************)
(*                                                                        *)
(*                              OCamlPro TypeRex                          *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*      (GNU Public Licence version 3.0).                                 *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
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
  iter (fun x y -> list := x :: !list) map;
  List.rev !list

let to_list2 map =
  let list = ref [] in
  iter (fun x y -> list := y :: !list) map;
  List.rev !list

exception MinElt
let exn_MinElt = MinElt

let min_elt map =
  let x = ref None in
  try
    iter (fun key v -> x := Some (key, v); raise exn_MinElt) map;
    None
  with MinElt -> !x


