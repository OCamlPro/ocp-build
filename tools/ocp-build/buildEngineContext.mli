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


open BuildEngineTypes

(* Add a new directory and all its sub-directories *)
val add_directory :
  build_context ->
  string -> build_directory

val find_directory : build_context -> string -> build_directory


(* [find_file dir basename] Find a file [basename] inside directory [dir].
   raise Not_found if the file could not be found. *)
val find_file : build_directory -> string ->   build_file
val find_dir : build_directory -> string -> build_directory



(*
  val new_rule :
(* context *)  build_context ->
(* location *)  string * int * string ->
(* main target *) build_file ->
(* actions *)  build_action list ->
  build_rule
*)

val add_file :
  build_context ->
  build_directory ->
  string -> build_file

val add_filename :
  build_context ->
  build_directory ->
  string -> build_file

val add_temp_file :
  build_context ->
  build_directory ->
  string -> build_file

val add_virtual_file :
  build_context ->
  build_directory ->
  string -> build_file

val make_virtual_file : BuildEngineTypes.build_file -> unit

val create : string -> string -> build_context
