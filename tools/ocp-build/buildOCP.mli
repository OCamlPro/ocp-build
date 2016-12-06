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



(* open BuildBase *)
open StringCompat
open BuildOCPTypes
open BuildValue.Types

type warning =
[ `MissingDirectory of string * string * string
| `PackageConflict of
    BuildOCPTypes.final_package * BuildOCPTypes.final_package *
      BuildOCPTypes.final_package
| `BadInstalledPackage of string * string
| `MissingDependency of string * string * string
| `KindMismatch of string * string * string * string
| `IncompletePackage of BuildOCPTypes.final_package
| `MissingPackage of string * BuildOCPTypes.final_package list
]


type state

val print_loaded_ocp_files : bool ref
val print_package_deps : bool ref
val print_missing_deps : bool ref

val init_packages : unit -> state
val load_ocp_files :
  config ->
  state -> File.t list -> int

val verify_packages :
  [> warning ] BuildWarnings.set ->
  state ->
  BuildOCPTypes.project

(* val load_project : File.t list -> project * int *)

(* returns the number of errors while reading the files *)

val find_root : File.t -> string list -> File.t

(*
  val save_project : File.t -> project -> unit
*)

val scan_root : File.t -> File.t list


(* [find_package pj file] returns the list of packages in
   project [pj] containing [file] as a source. *)
val find_package : project -> File.t -> final_package list


val save_project_state : project -> File.t -> unit
val load_project_state : File.t -> project

val find_obuild : (string -> unit) -> string -> unit


val empty_config : unit -> config
val generated_config : unit -> config

val print_conflict :
  'a BuildOCPTypes.package ->
  'b BuildOCPTypes.package ->
  'c BuildOCPTypes.package -> unit

(* formerly in buildOCPInterp.mli *)

val string_of_package_type : BuildOCPTypes.package_type -> string

val continue_on_ocp_error : bool ref

val initial_state : unit -> state
val copy_state : state -> state
val final_state : state -> final_package array
val filesubst : (string * env list) StringSubst.M.subst

val new_package :
  state ->
  string (* name *) ->
  string (* dirname *) ->
  string (* filename *) ->
  (string * Digest.t option) list (* filenames *) ->
  BuildOCPTypes.package_type ->
  env ->
  BuildOCPTypes.pre_package

val primitives_help : unit -> string list StringMap.t
