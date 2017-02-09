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

val number_of_cores : unit -> int
val split_version : string -> string * string * string
val find_in_PATH : string -> string list -> string
val get_PATH : unit -> string list
val set_PATH : string list -> unit

val find_first_in_path :
  string list -> (string -> bool) -> string list -> string option
