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

(** Extension of the stdlib Stream module *)

open Stream

(** Convert a token stream into a token list *)
val to_list : 'a t -> 'a list

(** Convert a list of lines into a char stream *)
val of_lines : string list -> char t

(** Check whether a stream is empty *)
val is_empty : 'a t -> bool
