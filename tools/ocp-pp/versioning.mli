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

type version = (version_sign * version_item) list

and version_item =
  | VInt of int
  | VString of string

and version_sign =
  | VPositive
  | VNegative

val version_of_string : string -> version
val string_of_version : version -> string

(* [compare v1 v2] returns:
   0 if v1 = v2
   1 if v1 < v2
  -1 if v1 > v2
*)
val version_compare : version -> version -> int
