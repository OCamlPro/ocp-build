(**************************************************************************)
(*                                                                        *)
(*   Typerex Libraries                                                    *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* This module implements a simple algorithm for substituting any
   string in any string. If several strings can be substituted at the
   same place, the one starting at the earliest position is chosen,
   and among different strings at the same position, the longest match
   is chosen.

   The current worst complexity is O(N.M) where N is the length of the
   argument string and M is the longest string that can be
   substituted. We should probably implement KMP at some point for
   applications where performance matters.
*)


(* The basic version: replace any string by any string *)

type subst

val empty_subst : unit -> subst

(* [add_to_subst subst src dst] mutates [subst] to add
 a transition from [src] to [dst]. *)
val add_to_subst : subst -> string -> string -> unit
(* [add_to_copy subst src dst] returns a copy of [subst] with a
   transition from [src] to [dst]. *)
val add_to_copy : subst -> string -> string -> subst

val subst_of_list : (string * string) list -> subst

val subst : subst -> string -> int * string
val iter_subst : subst -> string -> int * string


(* A generic version, where:
   * the substitution takes a ['context] as argument, from where the
     replacements will be computed at substitution time
   * when adding a string to replace, a function is provided that will
     compute the replacement from the given context
*)

module M : sig

  type 'a subst

  val empty_subst : unit -> 'a subst

(* [add_to_subst subst src dst] mutates [subst] to add
 a transition from [src] to [dst]. *)
  val add_to_subst : 'a subst -> string -> ('a -> string) -> unit

(* [add_to_copy subst src dst] returns a copy of [subst] with a
   transition from [src] to [dst]. *)
  val add_to_copy : 'a subst -> string -> ('a -> string) -> 'a subst


  val subst_of_list : (string * ('a -> string)) list -> 'a subst
  val subst : 'a subst -> string -> 'a -> int * string
  val iter_subst : 'a subst -> string -> 'a -> int * string

end

(* A specific version where:
 * the substitution is created by providing an array of strings to be replaced
 * the replacements are provided only at the substitution time, by providing
      another array of strings
*)
module Static : sig
  type t
  val create : string array -> t
  val subst : t -> string array -> string -> int * string
  val iter_subst : t -> string array -> string -> string

end
