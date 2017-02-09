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

val arg_list : (string * Arg.spec * string) list
val subcommand : BuildArgs.subcommand
val old_subcommand : BuildArgs.subcommand

val make_doc_targets : bool ref
val make_test_targets : bool ref
val make_build_targets : bool ref


val do_build :
  unit ->
  BuildActions.project_info *
    BuildTypes.builder_context * (module BuildTypes.Package) list *
  (module BuildTypes.Package) StringCompat.StringMap.t

(* val do_read_env : BuildActions.project_info -> BuildOCPInterp.state *)


val get_ncores : BuildOptions.config_input -> int
(* val finally_do : (unit -> unit) list ref  *)
