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

(* This is supposed to be a generic interface for reading/writing files.
  Normally, we would have the same module for Python syntax.
  Unfortunately:
  * we have not yet completely abstracted away the syntax from SimpleConfig,
    there are still some OCaml-like specific stuff (like comments)
  * it is not clear if we can split printing in the same way, between
    beginner and advanced options, in the Python syntax, as final options
    might be added to the last defined section instead of the DEFAULT section.
*)


val parse : File.t -> in_channel -> SimpleConfigTypes.option_module

val reset : unit -> unit
val save_module :
  bool ->
  string ->
  Buffer.t ->
  (string list * string * SimpleConfigTypes.option_value) list ->
  unit

val save_value :
  string -> Buffer.t -> SimpleConfigTypes.option_value -> unit

val save_binding :
  Buffer.t -> string -> SimpleConfigTypes.option_value -> unit
