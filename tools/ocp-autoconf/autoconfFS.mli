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

type oc
val open_out : string -> oc
val output_string : oc -> string -> unit
val fprintf : oc -> ('b, Buffer.t, unit) format -> 'b
val close_out : oc -> unit

(* [write_file ?exe filename content] *)
val write_file : ?exe:bool -> string -> string -> unit

(* [commit commit_filename] commit all changes, and return the list of
   modified files. *)
val commit : string -> string list

(* [add_post_commit_hook f] calls [f] when the files are committed *)
val add_post_commit_hook : (unit -> string list) -> unit
