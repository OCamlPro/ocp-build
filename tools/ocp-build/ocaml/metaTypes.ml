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

(* Raw-version of META files: *)

type precondition = string * bool

type variable = {
  var_name : string;
  mutable var_assigns : (precondition list * string) list;
  mutable var_additions : (precondition list * string) list;
}

type raw_meta = {
  p_parent : raw_meta option;
  mutable p_packages : (string * raw_meta) list;
  mutable p_variables : variable StringMap.t;
}



(* Simplified/partial pre-interpreted version of META files, as used
   by ocp-build 1.X: *)
type meta = {
  mutable meta_version : string option; (* USED *)
  mutable meta_description : string option;
  mutable meta_exists_if : string list; (* USED *)
  mutable meta_directory : string option; (* USED *)
  mutable meta_preprocessor : string option;
  mutable meta_name : string option;
  mutable meta_linkopts : string option;
  mutable meta_license : string option;
(*  mutable meta_browse_interfaces : string list; *)

  mutable meta_requires : string list var StringMap.t; (* USED *)
  mutable meta_archive : string list var StringMap.t; (* USED *)
  mutable meta_plugin : string list var StringMap.t;
  mutable meta_error : string var StringMap.t;

  mutable meta_package : (string * meta) list;
}

and 'a var = {
  metavar_key : string;
  metavar_preds : (string * bool) list;
  mutable metavar_value : 'a;
}




(* Some stats:
      1 IDENT[license]
      1 IDENT[descriptions]
      2 IDENT[predicates]
      3 IDENT[plugin_name]
      3 IDENT[plugin_synopsis]
      3 IDENT[plugin_system]
      5 IDENT[type_of_threads]
      6 IDENT[preprocessor]
     16 IDENT[linkopts]
     27 IDENT[name]
     49 IDENT[browse_interfaces]
     75 IDENT[directory]
    180 IDENT[exists_if]
    457 IDENT[requires]
    464 IDENT[version]
    474 IDENT[description]

     18 IDENT[error]
     40 IDENT[requires]
   1313 IDENT[archive]
*)
