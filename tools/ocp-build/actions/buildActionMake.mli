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

val arg_list : Arg.arg_list

val subcommand : Arg.command
val old_subcommand : Arg.command

val make_doc_targets : bool ref
val make_test_targets : bool ref
val make_build_targets : bool ref


val do_build :
  unit ->
  BuildActions.project_info *
    BuildTypes.builder_context * (module BuildTypes.Package) list *
  (module BuildTypes.Package) OcpCompat.StringMap.t

(* val do_read_env : BuildActions.project_info -> BuildOCPInterp.state *)


val get_ncores : BuildOptions.config_input -> int
(* val finally_do : (unit -> unit) list ref  *)
