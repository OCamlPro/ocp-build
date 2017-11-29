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

exception MissingSourceWithNoBuildingRule of BuildEngineTypes.build_rule * string

(* [init targets] Initialize the build engine, by checking activating
 all the rules needed for the creation of the files [targets].  raise
 MissingSourceWithNoBuildingRule (rule, filename) if a file is needed
 as a source and no rule is available to generate it.
 *)

val init :
  BuildEngineTypes.build_context ->
  BuildEngineTypes.build_file list ->
  unit

(* [fatal_errors ctx] returns the list of errors *)
val fatal_errors : BuildEngineTypes.build_context ->
                   BuildEngineTypes.fatal_error list

(* [parallel_loop ncores] Start the build process on [ncores] cores. *)
val parallel_loop :
  BuildEngineTypes.build_context -> int -> unit


val sanitize :
  BuildEngineTypes.build_context ->
  BuildEngineTypes.delete_orphans ->
  (string -> bool) -> (* return false on basename if topdir is orphan *)
  int
