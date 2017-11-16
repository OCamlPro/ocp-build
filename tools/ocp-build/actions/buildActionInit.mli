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

open Ezcmd.Modules

val subcommand : Arg.command

val action : unit -> unit
val arg_list : Arg.arg_list

                                            (*

val load_initial_project :
  BuildWarnings.set ->
  BuildActions.project_info ->
  BuildOCP.state ->
  BuildValue.TYPES.config_state ->

  BuildTypes.builder_context *
    (module BuildTypes.Package) OcpCompat.StringMap.t *
    BuildOCPTypes.project

val init_env :
  unit ->
    BuildWarnings.set *
    BuildActions.project_info * BuildOCP.state *
    BuildOCPTypes.project * BuildValue.TYPES.config_state

val chdir_to_project : BuildActions.project_info -> unit
                                             *)
