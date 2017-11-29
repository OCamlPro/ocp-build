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

exception NoWorkspace of string

(* Find the root of the current workspace *)
val lookup_root :
  root:string option -> local:bool -> workspace:string ->
  string (* root *)

(* scan the workspace looking for `cop-project` and `FILE.cop` files *)
val scan_workspace :
  string ->       (* root *)
    string list   (* project files *)
  * string list   (* module files *)
