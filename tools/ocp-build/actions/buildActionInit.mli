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

open StdlibArg

val subcommand : BuildArgs.subcommand
val action : unit -> unit
val arg_list : (string * Arg.spec * string) list

val load_initial_project :
  BuildWarnings.set ->
  BuildActions.project_info ->
  BuildOCP.state ->
  BuildTypes.builder_context *
    (module BuildTypes.Package) StringCompat.StringMap.t

val init_env :
  unit ->
    BuildWarnings.set *
    BuildActions.project_info * BuildOCP.state *
    BuildOCPTypes.project

val chdir_to_project : BuildActions.project_info -> unit
