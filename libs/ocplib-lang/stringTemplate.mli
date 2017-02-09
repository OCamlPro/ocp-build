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

type t
exception LabelMismatch of t * string
exception MissingLabel of t * string

val create : string Lazy.t ->
  string list ->
  (string * string) list option ->
  t
val load : t -> (string * string) list -> string

val labels : t -> string list
val content : t -> string
