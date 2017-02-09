(**************************************************************************)
(*                                                                        *)
(*   Typerex Tools                                                        *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU General Public License version 3 described in the file       *)
(*   LICENSE.                                                             *)
(*                                                                        *)
(**************************************************************************)

type set

type 'a warning = set -> 'a -> unit

val empty_set : unit -> set
val add : set -> string -> unit
val iter : (string -> unit) -> set -> unit
val count : set -> int
val sort : set -> unit
val equal : set -> set -> bool
val copy : set -> set
val clear : set -> unit
val diff : set -> set -> set

val wprintf : set -> ('b, unit, string, unit) format4 -> 'b
