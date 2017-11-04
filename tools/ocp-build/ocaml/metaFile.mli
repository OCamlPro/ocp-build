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
val file_of_raw : string -> MetaTypes.raw_meta -> unit

(* [variable_of_package p var_name preconds] returns a list of strings
   corresponding to the value of [var_name] as defined in the package
   [p] with the predicates [preconds]. The variable is initialized with
   the empty list, which is returned if the value is not set in the
   package. *)
val variable_of_raw :
  MetaTypes.raw_meta ->    (* META *)
  string ->                    (* wanted variable *)
  OcpCompat.StringSet.t ->  (* predicates *)
  string list                  (* result *)

 (* comma or space separated *)

val empty : unit -> MetaTypes.meta
val create_meta_file : string -> MetaTypes.meta -> unit
val add_requires : MetaTypes.meta -> (string * bool) list -> string list -> unit
val add_archive : MetaTypes.meta -> (string * bool) list -> string list -> unit
val add_plugin : MetaTypes.meta -> (string * bool) list -> string list -> unit
val split_simplify : string -> string list

val meta_of_raw : MetaTypes.raw_meta -> MetaTypes.meta
