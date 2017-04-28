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

       (* associate a name with a value, useful for debugging *)
       val name : t -> string
       val verbose : int -> bool
     end) ->
      sig
        val sort : M.t list ->
                   M.t list * (* sorted list *)
                     (M.t * M.t list * M.t list) list *  (* a cycle *)
                       M.t list (* other non-sorted nodes *)

      end
