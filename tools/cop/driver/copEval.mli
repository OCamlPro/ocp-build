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

type state
type package_description

val add_prim :
  string ->
  string list ->
  (BuildValue.TYPES.location ->
   state ->
   BuildValue.TYPES.config ->
   BuildValue.TYPES.value list -> BuildValue.TYPES.value) ->
  unit

val eval_file :
  state ->
  BuildValue.TYPES.config ->
  string ->
  BuildValue.TYPES.config

val init_state : unit -> state
val init_workspace :
  state ->
  string list ->
  state * BuildValue.TYPES.config

val load_projects :
  state ->
  BuildValue.TYPES.config ->
  string list ->
  int * package_description list

val add_project:
  state ->
  BuildValue.TYPES.location ->
  string ->
  BuildValue.TYPES.config ->
  BuildValue.TYPES.value list ->
  BuildValue.TYPES.env ->
  unit
