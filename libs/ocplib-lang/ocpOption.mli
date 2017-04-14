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

(** Functions over the Option type *)

(** Map a function over an optional value *)
val map : ('a -> 'b) -> 'a option -> 'b option

(** Return either the value contained in an optional value, or a
    default value *)
val default : 'a -> 'a option -> 'a

(** Iter a function over an optional value *)
val iter : ('a -> unit) -> 'a option -> unit
