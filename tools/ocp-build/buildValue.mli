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

open OcpCompat

module TYPES : sig

  exception Var_not_found of string

  type location = {
    loc_begin : Lexing.position;
    loc_end : Lexing.position;
  }

  (* Two kinds of strings: StringVersion only differs in comparison,
     where a versioning comparison is used. *)
  type string_kind =
  | StringRaw
  | StringVersion

  type env = { env : value StringMap.t }
  and value =
  | VList of value list
  | VObject of env
  | VString of string * string_kind
  | VTuple of value list
  | VBool of bool
  | VInt of int
  | VFun of functional_value

  and functional_value =
  | VFunction of (location -> value list -> value)
  | VPrim of string

  (* To avoid dealing with dependencies, modules can be declared lazily
     by provides("Mod", function(){...}), in which case the function will
     only be executed on demand. When doing so, we set the module to
     Computing to avoid a recursion. *)
  type module_desc =
  | Declared of value
  | Computing
  | Computed of value

  type config_state = {
    mutable cfs_modules : (module_desc ref * Versioning.version) StringMap.t;
    mutable cfs_store : value StringMap.t;
  }

  (* The configuration at a package definition site *)
  type config = {
    config_env : env;
    config_state : config_state;
    config_dirname : string;
    config_filename : string;
    config_filenames : (string * Digest.t option) list;
  }


(* Just for compatibility: a plist is morally a
   VList of VPair (VString * VObject) *)
  type plist = value
  type prop_list = (string * env) list

  exception NotAPropertyList

  type 'a source_option = {
    get : env list -> 'a;
    set : 'a -> unit;
  }


end

open TYPES

val string_of_value : value -> string
val string_of_env : env -> string

val prop_list : value -> prop_list
val value : prop_list -> value

val empty_env : env

val set_global : string -> value -> unit
val get_global : string -> plist

val bool_of_plist : value -> bool
val plist_of_bool : bool -> plist

val strings_of_plist : value -> string list
val plist_of_strings : string list -> plist

val string_of_plist : value -> string
val plist_of_string : string -> plist

val string_option_of_plist : value -> string option
val plist_of_string_option : string option -> plist

val set : env -> string -> value -> env
(* [get envs s] get from local envs, or from global env *)
val get : env list -> string -> plist
val get_with_default : env list -> string -> value -> plist
(* [get_local envs s] get only from local envs *)
val get_local : env list -> string -> plist
val get_local_with_default : env list -> string -> value -> plist

val get_local_prop_list : env list -> string -> prop_list
val get_local_prop_list_with_default : env list -> string -> prop_list -> prop_list

val set_bool : env -> string -> bool -> env
val get_bool : env list -> string -> bool
val get_bool_with_default : env list -> string -> bool -> bool
val get_local_bool : env list -> string -> bool
val get_local_bool_with_default : env list -> string -> bool -> bool

val set_strings : env -> string -> string list -> env
val get_strings : env list -> string -> string list
val get_strings_with_default : env list -> string -> string list -> string list
val get_local_strings : env list -> string -> string list
val get_local_strings_with_default : env list -> string -> string list -> string list

val set_string : env -> string -> string -> env
val get_string : env list -> string -> string
val get_string_with_default : env list -> string -> string -> string
val get_local_string : env list -> string -> string
val get_local_string_with_default : env list -> string -> string -> string

val set_string_option : env -> string -> string option -> env
val get_string_option : env list -> string -> string option
val get_string_option_with_default : env list -> string -> string option -> string option
val get_local_string_option : env list -> string -> string option
val get_local_string_option_with_default : env list -> string -> string option -> string option

val set_path : env -> string -> string -> env
val get_path : env list -> string -> string
val get_path_with_default : env list -> string -> string -> string
val get_local_path : env list -> string -> string
val get_local_path_with_default : env list -> string -> string -> string

val is_already_installed : env list -> bool

val new_option : string -> value -> value source_option
val new_bool_option : string -> bool -> bool source_option
val new_strings_option : string -> string list -> string list source_option
val new_string_option : string -> string -> string source_option
val new_version_option : string -> string -> string source_option

val iter_env : (string -> value -> unit) -> env -> unit

val bprint_env : Buffer.t -> string -> env -> unit
val bprint_value : Buffer.t -> string -> value -> unit

val empty_config : unit -> config
val config_get : config -> string -> value
val config_set : config -> string -> value -> config

val unit : value
val noloc : string -> location
val string_of_location : location -> string

val set_deep_field : TYPES.env -> string list -> TYPES.value -> TYPES.env

val new_object : (string * value) list -> value
val compare_values : value -> value -> int
val empty_config_state : unit -> TYPES.config_state

val fold :
  (string -> TYPES.value -> 'a -> 'a) -> TYPES.env -> 'a -> 'a
