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

(* [load_dependencies filename] returns a list of association between
 a target and a list of filenames, its dependencies. *)
val load_dependencies : string -> (string * string list list) list

(* [load_modules_dependencies filename] returns a list of association between
 a target and a list of filenames, its dependencies. *)
val load_modules_dependencies :
  BuildOCamlTypes.ocaml_package -> BuildValue.TYPES.env ->
  BuildOCamlTypes.force_kind ->
  BuildEngineTypes.build_directory -> string list -> (* needs_odoc *) bool ->
  string -> (string * string list list) list


val modname_of_file : BuildValue.TYPES.env list ->
  BuildOCamlTypes.force_kind ->
  string ->
  bool * (* is_ml *)
    string * (* modname *)
    string   (* basename *)

val load_make_dependencies : string -> (string * string list) list
