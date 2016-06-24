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

type warning = [
| `SyntaxDepDeclaredAsNotSyntax of string * string * string
| `SyntaxDepNotDeclared of string * string * string
]

val get_pp :
  [> warning ] BuildWarnings.set ->
  BuildOCamlTypes.ocaml_package ->
  string -> (* source basename *)
  BuildValue.Types.env ->
  BuildOCamlTypes.pp


(* Should probably be in BuildOCamlMisc *)
val add_pp_requires :
  BuildEngineTypes.build_rule -> BuildOCamlTypes.pp -> unit

val get_tool_requires :
  [> warning ] BuildWarnings.set ->
  string ->
  BuildOCamlTypes.ocaml_package ->
  string list ->
  BuildEngineTypes.build_file list
