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

(** This module implements {e tries}. *)

(** Given a map [M] over an arbitrary type [M.key], the following
    functor constructs a new map over type [M.key list]. *)
module Make(M : Map.S) : (Map.S with type key = M.key list)
