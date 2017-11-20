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

val init : unit -> unit
val begin_command :
  BuildEngineTypes.build_context ->
  BuildEngineTypes.build_process -> unit
val end_command :
  BuildEngineTypes.build_context ->
  BuildEngineTypes.build_process ->
  float ->
  int ->
  unit

val add_error :
  BuildEngineTypes.build_context ->
  BuildEngineTypes.error -> unit
val has_error :
    BuildEngineTypes.build_context -> bool
val errors :   BuildEngineTypes.build_context -> BuildEngineTypes.error list
val finish : unit -> unit

val eprint_context :
  BuildEngineTypes.build_context -> unit

val strings_of_error : BuildEngineTypes.error -> string list
val strings_of_fatal_error : BuildEngineTypes.fatal_error -> string list

val print_rule : BuildEngineTypes.build_rule -> unit
val print_indented_command : BuildEngineTypes.build_action -> unit

val string_of_rule_state : BuildEngineTypes.build_rule -> string

val print_loc : BuildEngineTypes.build_loc -> unit
val string_of_loc : BuildEngineTypes.build_loc -> string
