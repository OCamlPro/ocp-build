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

open StringCompat
open BuildValue.Types
open BuildOCP2Tree

val fatal_error : BuildValue.Types.location ->
  ('a, unit, string, 'b) format4 -> 'a
val warning : location -> ('a, unit, string, unit) format4 -> 'a

val raise_type_error :
  location ->
  string -> int -> string -> BuildValue.Types.value -> 'a

val raise_bad_arity :
  location ->
  string -> int -> BuildValue.Types.value list -> 'a

val ocp2_raise :
  location -> string -> BuildValue.Types.value -> 'a

module Init(S: sig

    type context

    val define_package :
      location ->
      context ->
      config ->
      name:string ->
      kind:string ->
      unit

    val filesubst : (string * env list) StringSubst.M.subst

  end) : sig
  val primitives :
    (
      (location -> S.context -> config -> value list -> value) *
      string list
    ) StringMap.t ref
  val add_primitive :
           string -> string list ->
           (BuildValue.Types.location ->
            S.context ->
            BuildValue.Types.config ->
            BuildValue.Types.value list -> BuildValue.Types.value) ->
           unit
  val primitives_help : unit -> string list StringCompat.StringMap.t

  end
