(**************************************************************************)
(*                                                                        *)
(*                              OCamlPro TypeRex                          *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the LGPL v2.1 with       *)
(*   the special exception on linking described in the file LICENSE.      *)
(*      (GNU Lesser General Public Licence version 2.1)                   *)
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
