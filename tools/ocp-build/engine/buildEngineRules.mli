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

val new_rule :
  BuildEngineTypes.build_context ->
  BuildEngineTypes.build_loc ->
  BuildEngineTypes.build_file -> (* main target *)
  BuildEngineTypes.build_action list -> (* actions *)
  BuildEngineTypes.build_rule

val add_rule_source :
  BuildEngineTypes.build_rule -> BuildEngineTypes.build_file -> unit

val add_rule_sources :
  BuildEngineTypes.build_rule ->
  BuildEngineTypes.build_file list -> unit

(* Files that are created temporarily by the rule. Such files are either deleted
  or moved later to their final location. They must be specified if there is a risk
  of conflict with other rules generating/using the same files. *)
val add_rule_temporary :
  BuildEngineTypes.build_rule -> BuildEngineTypes.build_file -> unit

val add_rule_temporaries :
  BuildEngineTypes.build_rule -> BuildEngineTypes.build_file list -> unit

val add_rule_target :
  BuildEngineTypes.build_rule -> BuildEngineTypes.build_file -> unit

val add_rule_targets :
  BuildEngineTypes.build_rule -> BuildEngineTypes.build_file list -> unit

val add_rule_time_dependency :
  BuildEngineTypes.build_rule -> BuildEngineTypes.build_file -> unit

val add_rule_command :
  BuildEngineTypes.build_rule ->
  BuildEngineTypes.build_action -> unit
val add_rule_commands :
  BuildEngineTypes.build_rule ->
  BuildEngineTypes.build_action list -> unit

val new_command :
  string list ->
  BuildEngineTypes.command_argument list ->
  BuildEngineTypes.build_command
val add_command_arg :
  BuildEngineTypes.build_command ->
  BuildEngineTypes.command_argument -> unit
val add_command_args :
  BuildEngineTypes.build_command ->
  BuildEngineTypes.command_argument list -> unit
val argument_of_string : string -> BuildEngineTypes.command_argument

val add_command_string :
  BuildEngineTypes.build_command -> string -> unit
val add_command_strings :
  BuildEngineTypes.build_command -> string list -> unit
val add_command_pipe :
  BuildEngineTypes.build_command -> string -> unit








val command_of_command :
  BuildEngineTypes.build_rule ->
  BuildEngineTypes.build_command -> string list

val string_of_argument : BuildEngineTypes.command_argument -> string
val file_of_argument :
  BuildEngineTypes.build_rule ->
  BuildEngineTypes.command_argument -> FileGen.t
val argument_of_argument :
  BuildEngineTypes.build_rule ->
  BuildEngineTypes.command_argument ->
  string

val rule_temp_dir : BuildEngineTypes.build_rule -> FileGen.t
