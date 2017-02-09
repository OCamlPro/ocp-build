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
  string list -> unit
val has_error :
    BuildEngineTypes.build_context -> bool
val errors :   BuildEngineTypes.build_context -> string list list
val finish : unit -> unit

val eprint_context :
  BuildEngineTypes.build_context -> unit
