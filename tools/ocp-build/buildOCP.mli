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

(* open BuildBase *)
open StringCompat
open BuildOCPTypes
open BuildValue.TYPES

val w_MissingDirectory : (string * string * string) BuildWarnings.warning
val w_PackageConflict :
    (BuildOCPTypes.pre_package * BuildOCPTypes.pre_package *
      BuildOCPTypes.pre_package) BuildWarnings.warning
val w_BadInstalledPackage :
  (string * string) BuildWarnings.warning
val w_MissingDependency : (string * string * string) BuildWarnings.warning
val w_KindMismatch : (string * string * string * string) BuildWarnings.warning
val w_IncompletePackage : (BuildOCPTypes.pre_package) BuildWarnings.warning
val w_MissingPackage : (string * BuildOCPTypes.pre_package list) BuildWarnings.warning



type state

val print_loaded_ocp_files : bool ref
val print_package_deps : bool ref
val print_missing_deps : bool ref

val init_packages : unit -> state
val load_ocp_files :
  config ->
  state -> File.t list -> int

val verify_packages :
  BuildWarnings.set ->
  state ->
  BuildOCPTypes.project

val get_packages : state -> BuildOCPTypes.pre_package IntMap.t
val plugin_verifiers : (BuildWarnings.set -> state -> unit) list ref

(* val load_project : File.t list -> project * int *)

(* returns the number of errors while reading the files *)

val find_root : File.t -> string list -> File.t

(*
  val save_project : File.t -> project -> unit
*)

val scan_root : File.t -> File.t list


(* [find_package pj file] returns the list of packages in
   project [pj] containing [file] as a source.
val find_package : project -> File.t -> final_package list
*)

val save_project_state : project -> File.t -> unit
val load_project_state : File.t -> project

val find_obuild : (string -> unit) -> string -> unit


val empty_config : unit -> config
val generated_config : unit -> config

val add_ocaml_package :
           (location -> state ->
            BuildValue.TYPES.config ->
            string -> BuildOCPTypes.package_type -> unit BuildOCPTypes.package)
           ref
val package_type_of_string : string -> BuildOCPTypes.package_type
val define_package :
  location ->
  state ->
  BuildValue.TYPES.config ->
  name:string ->
  kind:BuildOCPTypes.package_type -> unit BuildOCPTypes.package

val add_primitive : (* only for OCP2 *)
  string ->
  string list ->
  (BuildValue.TYPES.location ->
   state ->
   BuildValue.TYPES.config ->
   BuildValue.TYPES.value list -> BuildValue.TYPES.value) ->
  unit

val print_conflict :
  'a BuildOCPTypes.package ->
  'b BuildOCPTypes.package ->
  'c BuildOCPTypes.package -> string

(* formerly in buildOCPInterp.mli *)

val string_of_package_type : BuildOCPTypes.package_type -> string

val continue_on_ocp_error : bool ref

val initial_state : unit -> state
val copy_state : state -> state
val final_state : state -> final_package array
val filesubst : (string * env list) StringSubst.M.subst

val new_package :
  location ->
  state ->
  string (* name *) ->
  string (* dirname *) ->
  string (* filename *) ->
  (string * Digest.t option) list (* filenames *) ->
  BuildOCPTypes.package_type ->
  env ->
  BuildOCPTypes.pre_package

val primitives_help : unit -> string list StringMap.t

val conf_add_disabled_package : string -> unit
