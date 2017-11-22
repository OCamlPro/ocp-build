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

open OcpCompat
open BuildValue.TYPES
open BuildOCP2Tree

val fatal_error : BuildValue.TYPES.location ->
  ('a, unit, string, 'b) format4 -> 'a
val warning : location -> ('a, unit, string, unit) format4 -> 'a

val raise_type_error :
  location ->
  string -> int -> string -> BuildValue.TYPES.value -> 'a

val raise_bad_arity :
  location ->
  string -> int -> BuildValue.TYPES.value list -> 'a

val ocp2_raise :
  location -> string -> BuildValue.TYPES.value -> 'a

module Init(S: sig

    type context

  end) : sig
  val primitives :
    (
      (location -> S.context -> config -> value list -> value) *
      string list
    ) StringMap.t ref
  val add_primitive :
           string -> string list ->
           (BuildValue.TYPES.location ->
            S.context ->
            BuildValue.TYPES.config ->
            BuildValue.TYPES.value list -> BuildValue.TYPES.value) ->
           unit
  val primitives_help : unit -> string list OcpCompat.StringMap.t

  val apply_fun :
    BuildValue.TYPES.functional_value ->
    BuildValue.TYPES.location ->
    S.context ->
    BuildValue.TYPES.config ->
    BuildValue.TYPES.value list -> BuildValue.TYPES.value

  end

val with_feature : string -> unit
val without_feature : string -> unit
val queried_features : unit -> bool StringMap.t
