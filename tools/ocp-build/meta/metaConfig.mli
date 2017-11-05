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

(* [load_config ()] calls "ocamlfind" to load its search path.
   [load_config ~ocamlfind ()] can be used to specify a specific
   ocamlfind executable. *)
val load_config : ?ocamlfind:string list -> unit -> string list
