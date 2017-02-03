(**************************************************************************)
(*                                                                        *)
(*                              OCamlPro TypeRex                          *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*      (GNU Public Licence version 3.0).                                 *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)

open StringCompat

module Types : sig

  type location = {
    loc_begin : Lexing.position;
    loc_end : Lexing.position;
  }

  type env = { env : value StringMap.t }
  and value =
  | VList of value list
  | VObject of env
  | VString of string
  | VTuple of value list
  | VBool of bool
  | VInt of int
  | VFunction of (location -> value list -> value)
  | VPrim of string

(* Just for compatibility: a plist is morally a
   VList of VPair (VString * VObject) *)
  type plist = value
  type prop_list = (string * env) list

  type 'a source_option = {
    get : env list -> 'a;
    set : 'a -> unit;
  }

  exception Var_not_found of string
  exception NotAPropertyList

  (* The configuration at a package definition site *)
  type config = {
    config_env : env;
    config_modules : (value * Versioning.version) StringMap.t ref;
    config_dirname : string;
    config_filename : string;
    config_filenames : (string * Digest.t option) list;
  }

end

open Types

val string_of_value : value -> string

val prop_list : value -> prop_list
val value : prop_list -> value

val empty_env : env

val set_global : string -> plist -> unit
val get_global : string -> plist

val bool_of_plist : plist -> bool
val plist_of_bool : bool -> plist

val strings_of_plist : plist -> string list
val plist_of_strings : string list -> plist

val string_of_plist : plist -> string
val plist_of_string : string -> plist

val string_option_of_plist : plist -> string option
val plist_of_string_option : string option -> plist

val set : env -> string -> plist -> env
val get : env list -> string -> plist
val get_with_default : env list -> string -> plist -> plist
val get_local : env list -> string -> plist
val get_local_with_default : env list -> string -> plist -> plist

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

val new_option : string -> plist -> plist source_option
val new_bool_option : string -> bool -> bool source_option
val new_strings_option : string -> string list -> string list source_option
val new_string_option : string -> string -> string source_option

val iter_env : (string -> plist -> unit) -> env -> unit

val bprint_env : Buffer.t -> string -> env -> unit
val bprint_value : Buffer.t -> string -> value -> unit

val empty_config : unit -> config
val config_get : config -> string -> value
val config_set : config -> string -> value -> config

val unit : value
val noloc : string -> location
val string_of_location : location -> string

val set_deep_field : Types.env -> string list -> Types.value -> Types.env
