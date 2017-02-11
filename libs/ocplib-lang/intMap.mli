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

include Map.S with type key = int

val to_list: 'a t -> (int * 'a) list
val to_list1: 'a t -> int list
val to_list2: 'a t -> 'a list

exception MinElt
val min_elt: 'a t -> (key * 'a) option
