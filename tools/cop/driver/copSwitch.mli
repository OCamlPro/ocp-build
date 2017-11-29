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
val init :
  string ->
  BuildValue.TYPES.config ->
  CopTypes.switch * BuildValue.TYPES.config
 *)

val parse_switch :
  CopTypes.context ->
  OcpCompat.StringMap.key ->
  BuildValue.TYPES.env -> unit

val eval_switch :
  BuildEngineTypes.build_context ->
  CopTypes.switch ->
  int ->
  CopTypes.package list ->
  unit

val has_switch : unit -> bool
