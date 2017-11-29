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

exception BadRule of BuildValue.TYPES.location * string * BuildValue.TYPES.value
exception BadRequire of BuildValue.TYPES.location * BuildValue.TYPES.value

val add_prim :
  string -> (* prim name *)
  string list -> (* help *)
  (* function: *)
  (BuildValue.TYPES.location ->
   CopTypes.context ->
   BuildValue.TYPES.config ->
   BuildValue.TYPES.value list -> BuildValue.TYPES.value) ->
  unit

val init_state : unit -> CopTypes.context

val eval_file :
  CopTypes.context ->
  BuildValue.TYPES.config ->
  string ->
  BuildValue.TYPES.config

val init_workspace :
  CopTypes.context ->
  string list ->
  CopTypes.context * BuildValue.TYPES.config

val load_projects :
  CopTypes.context ->
  CopTypes.switch ->
  BuildValue.TYPES.config ->
  string list ->
  int

val add_project:
  CopTypes.context ->
  BuildValue.TYPES.location ->
  string -> (* project name *)
  BuildValue.TYPES.config ->
  BuildValue.TYPES.value ->
  BuildValue.TYPES.value ->
  unit

val parse_rule :
  CopTypes.package ->
  BuildValue.TYPES.location ->
  BuildValue.TYPES.value -> CopTypes.rule
