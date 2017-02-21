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

(** Extension of the stdlib Digest module *)

open Digest

(** Return the printable hexadecimal representation of the given digest. *)
val to_hex : t -> string

(** Return the digest corresponding to the printable hexadecimal representation. *)
val of_hex : string -> t
val from_hex : string -> t

(** Return the digest by interpreting the string as a raw digest *)
val of_direct_string : string -> t

(** Return the string with the raw digest inside *)
val to_direct_string : t -> string
