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


open StringCompat

exception IgnoreDirectory

val ignore_file_or_directory : unit -> 'a

val scan_directory_for_suffix :
 (* directory *) string -> (* extension *) string ->
  (string -> unit) -> unit

val scan_directory_for_files :
 (* directory *) string ->
 (* extensions handlers *)
  (string -> unit) StringMap.t ->
  unit

val scan_directory_for_extensions :
 (* directory *) string ->
 (* extensions handlers *)
  (string -> unit) StringMap.t ->
  unit

(*
val scan_directory_for_extensions2 :
 (* directory *) string ->
 (* extensions handlers *)
  (string ->  (* relative filename *)
   string ->  (* full filename *)
   unit) StringMap.t ->
  unit
*)


val scan_directory : (string (*dirname*) ->
                      string (*basename*) ->
                      string (*fullname*) -> unit)
  -> string -> unit
