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


open BuildOCPTypes (* for package_type *)

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
  | StmtDefinePackage of package_type * expression * statement list
  | StmtIfThenElse of condition * statement list * statement list option
  | StmtInclude of expression * statement list * statement list option

val modname_of_fullname : string -> string
val string_of_package_type : BuildOCPTypes.package_type -> string
