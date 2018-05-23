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

type version_op =
  | EqVersion
  | LtVersion (* < VERSION *)
  | LeVersion (* <= VERSION *)
  | GtVersion (* > VERSION *)
  | GeVersion (* >= VERSION, default *)

type package = {
  name : string;
  version : (version_op * string) option;
  opam : string option;
}

let string_of_version_op = function
  | EqVersion -> "="
  | GeVersion -> ">="
  | GtVersion -> ">"
  | LeVersion -> "<="
  | LtVersion -> "<"
