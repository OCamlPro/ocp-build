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

val w_SyntaxDepDeclaredAsNotSyntax:
  BuildWarnings.set -> string * string * string -> unit
val w_SyntaxDepNotDeclared:
  BuildWarnings.set -> string * string * string -> unit

val get_pp :
  string -> (* special: asm, byte, dep, mli *)
   BuildWarnings.set ->
  BuildOCamlTypes.ocaml_package ->
  string -> (* source basename *)
  BuildValue.TYPES.env ->
  BuildOCamlTypes.pp


(* Should probably be in BuildOCamlMisc *)
val add_pp_requires :
  BuildEngineTypes.build_rule -> BuildOCamlTypes.pp -> unit

val get_tool_requires :
  BuildWarnings.set ->
  string ->
  BuildOCamlTypes.ocaml_package ->
  string list ->
  BuildEngineTypes.build_file list
