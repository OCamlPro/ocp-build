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

type position = int * int * int (* (line number, line pos, char pos) *)
type location = string * position * position

type kind =
  Type of string list
| Ident of string

type ident =
  Def of string * location (* location of scope, end_pos = -1 => unlimited *)
| ExtRef of string
| IntRef of string * location (* location of definition *)

type annot_file = {
  annot_filenames : string list;
  annot_infos : (location * kind list) list;
}

val parse_file :  string -> annot_file
val parse_ident : string -> ident
