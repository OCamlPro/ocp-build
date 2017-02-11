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

(*
val add_dependency_loader :
  string -> (string -> ( string * string list list) list) -> unit
val find_dependency_loader :
  string -> (string -> ( string * string list list) list)
*)

val new_dir_id : BuildEngineTypes.build_context -> int
val new_file_id : BuildEngineTypes.build_context -> int
val new_rule_id : BuildEngineTypes.build_context -> int
val new_process_id : BuildEngineTypes.build_context -> int

val file_filename : BuildEngineTypes.build_file -> string
(* val print_indented_command : BuildEngineTypes.build_action -> unit *)


