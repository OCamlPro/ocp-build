(**************************************************************************)
(*                                                                        *)
(*                              OCamlPro TypeRex                          *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the LGPL v2.1 with       *)
(*   the special exception on linking described in the file LICENSE.      *)
(*      (GNU Lesser General Public Licence version 2.1)                   *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
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

module Static : sig
  type t
  val create : string array -> t
  val subst : t -> string array -> string -> int * string
  val iter_subst : t -> string array -> string -> string

end
