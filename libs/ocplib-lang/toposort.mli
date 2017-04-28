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

(*
(** Topological Sort *)


(** Abstract type for a node *)
type node

(** Node creation *)
val new_node : unit -> node

module Make :
  functor
    (M : sig
      type t
      val node : t -> node
      val iter_edges : (t -> unit) -> t -> unit
    end) ->
      sig
        exception RecursiveDependency of M.t
        val sort : M.t list -> M.t list
        val sort_sorted : M.t list -> M.t list
      end
 *)
