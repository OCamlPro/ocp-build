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



(* [load_dependencies filename] returns a list of association between
 a target and a list of filenames, its dependencies. *)
val load_dependencies : string -> (string * string list list) list

(* [load_modules_dependencies filename] returns a list of association between
 a target and a list of filenames, its dependencies. *)
val load_modules_dependencies :
  BuildOCamlTypes.ocaml_package -> BuildValue.Types.env ->
  BuildOCamlTypes.force_kind ->
  BuildEngineTypes.build_directory -> string list -> (* needs_odoc *) bool ->
  string -> (string * string list list) list


val modname_of_file : BuildValue.Types.env list ->
  BuildOCamlTypes.force_kind ->
  string -> bool * string * string

val load_make_dependencies : string -> (string * string list) list
