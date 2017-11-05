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
type preconditions = precondition list

type variable = {
  var_name : string;
  mutable var_assigns : (preconditions * string) list;
  mutable var_additions : (preconditions * string) list;
}

type t = {
  p_parent : t option;
  mutable p_packages : (string * t) list;
  mutable p_variables : variable StringMap.t;
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
