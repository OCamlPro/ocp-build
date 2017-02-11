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

(** Extension of the stdlib Hashtbl module *)

open Hashtbl

(** Convert an hash-table into a list *)
val to_list : ('a, 'b) t -> ('a * 'b) list

(** Convert a list into an hash-table *)
val of_list : ('a * 'b) list -> ('a, 'b) t

(** Increments the integer value associated to a key *)
val incr : ('a, int) t ->  'a -> unit

(** Check whether a predicate holds on all key-value pairs of an
    hash-table *)
val for_all : ('a, 'b) t -> ('a -> 'b -> bool) -> bool

(** Check wether a predicate holds on at least one key-value pair of
    an hash-table *)
val exists : ('a, 'b) t -> ('a -> 'b -> bool) -> bool
