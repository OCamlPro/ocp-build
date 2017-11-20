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

open MetaTypes

val file_of_meta : string -> MetaTypes.t -> unit
val string_of_meta : MetaTypes.t -> string
val meta_of_file : string -> MetaTypes.t

(* [variable_of_package p var_name preconds] returns a list of strings
   corresponding to the value of [var_name] as defined in the package
   [p] with the predicates [preconds]. The variable is initialized with
   the empty list, which is returned if the value is not set in the
   package. *)
type predicates
val variable_of_meta :
  MetaTypes.t ->    (* META *)
  string ->                    (* wanted variable *)
  predicates ->  (* predicates *)
  string list                  (* result *)

val preds_none : predicates
val preds_of_strings : string list -> predicates

val preds_byte : predicates
val preds_asm : predicates

val split_list : string list -> string list

val directory : MetaTypes.t -> string list
val exists_if : MetaTypes.t -> string list
val version : MetaTypes.t -> string list
val archive : MetaTypes.t -> predicates -> string list (* already splitted *)
val requires : MetaTypes.t -> predicates -> string list (* already splitted *)

val create : MetaTypes.t option -> MetaTypes.t
val set_directory : MetaTypes.t -> string -> unit
val set_version : MetaTypes.t -> string -> unit
val set_description : MetaTypes.t -> string -> unit
val set_exists_if : MetaTypes.t -> string -> unit

val precs_byte : preconditions
val precs_asm : preconditions

val add_archive : MetaTypes.t -> MetaTypes.preconditions -> string -> unit
val add_plugin : MetaTypes.t -> MetaTypes.preconditions -> string -> unit
val add_requires : MetaTypes.t -> MetaTypes.preconditions -> string -> unit

 (* comma or space separated

val empty : unit -> MetaTypes.t
val create_meta_file : string -> MetaTypes.t -> unit
val add_requires : MetaTypes.t -> (string * bool) list -> string list -> unit
val add_archive : MetaTypes.t -> (string * bool) list -> string list -> unit
val add_plugin : MetaTypes.t -> (string * bool) list -> string list -> unit
val split_simplify : string -> string list

val meta_of_meta : MetaTypes.t -> MetaTypes.t
  *)
