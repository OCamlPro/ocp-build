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

val putenv : string -> string -> unit
val add_to_global_subst : string -> string -> unit
val subst_global : string -> string

val global_subst : unit -> string StringMap.t
val map_subst : string StringMap.t -> string -> string

type 'a t

val create_substituter : (string * ('a -> string)) list -> 'a t
val apply_substituter : 'a t -> string -> 'a -> string


(* [substitute f ctx string] replaces all occurrences of variables in
  the form `%{...}%` in `string` by calling `f ctx variable` with the
  name of the variable.
 *)

val substitute : ('context -> string -> string) -> 'context -> string -> string
