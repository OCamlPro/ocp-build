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

(*open BuildOCPTypes (* for package_type *) *)

type camlpN =
  | Camlp4
  | Camlp5

and condition =
  | IsEqual of expression * expression
  | IsNonFalse of expression
  | Greater of expression * expression
  | GreaterEqual of expression * expression
  | NotCondition of condition
  | AndConditions of condition * condition
  | OrConditions of condition * condition

and expression =
  | ExprApply of expression * set_option_list
  | ExprString of string
  | ExprVariable of string
  | ExprPrimitive of string * set_option_list
  | ExprList of expression list
  | ExprBool of bool

and set_option_list = set_option list

and set_option =
  | OptionVariableSet of string * expression
  | OptionVariableAppend of string * expression
  | OptionConfigUse of expression
  | OptionIfThenElse of condition * set_option * set_option option
  | OptionBlock of set_option list
(*  | OptionConfigAppend of string *)

type statement =
    StmtOption of set_option
  | StmtBlock of statement list
  | StmtDefineConfig of expression * set_option list
  | StmtDefinePackage of string * expression * statement list
  | StmtIfThenElse of condition * statement list * statement list option
  | StmtInclude of expression * statement list * statement list option

val modname_of_fullname : string -> string
