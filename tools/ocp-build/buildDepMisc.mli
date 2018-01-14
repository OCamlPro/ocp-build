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

(* [load_dependencies filename] returns a list of association between
 a target and a list of filenames, its dependencies. *)
val load_dependencies : string -> (string * string list list) list

val load_make_dependencies : string -> (string * string list) list

val print_dependencies : (string * string list list) list -> unit
