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

      (*
let modname_of_fullname fullname =
  let modname = Filename.chop_extension (Filename.basename fullname) in
  modname.[0] <- Char.uppercase modname.[0];
  modname

let string_of_package_type kind =
  match kind with
    ProgramPackage -> "program"
  | LibraryPackage -> "library"
  | SyntaxPackage -> "syntax"
          (*	  | ProjectToplevel -> "toplevel" *)
  | ObjectsPackage -> "objects"
  | TestPackage -> "test"
  | RulesPackage -> "rules"


let rec string_of_condition cond =
  match cond with
  | IsEqual (exp1, exp2) ->
    Printf.sprintf "%s = %s"
      (string_of_expression exp1) (string_of_expression exp2)
  | IsNonFalse exp ->
    Printf.sprintf "%s" (string_of_expression exp)
  | NotCondition cond ->
    Printf.sprintf "not ( %s )" (string_of_condition cond)
  | AndConditions (cond1, cond2) ->
    Printf.sprintf "( %s ) && ( %s )" (string_of_condition cond1) (string_of_condition cond2)
  | OrConditions (cond1, cond2) ->
    Printf.sprintf "( %s ) || ( %s )" (string_of_condition cond1) (string_of_condition cond2)

and string_of_expression exp =
  match exp with
  | ExprString s -> Printf.sprintf "%S" s
  | ExprVariable s -> s
  | ExprList [] -> "[]"
  | ExprList [ ExprApply (ExprString "", [ OptionVariableSet ("type", ExprString "%bool") ])] -> "true"
  | ExprList explist ->
    Printf.sprintf "[ %s ]"
      (String.concat "; " (List.map string_of_expression explist))
  | ExprPrimitive (s, options) ->
    Printf.sprintf "%%%s (%s)" s
      (String.concat ";" (List.map string_of_set_option options))
  | ExprApply (e, options) ->
    Printf.sprintf "%s (%s)"
      (string_of_expression e)
      (String.concat ";" (List.map string_of_set_option options))

and string_of_set_option option =
  match option with
  | OptionVariableSet (v, exp) ->
    Printf.sprintf "%s = %s" v (string_of_expression exp)
  | OptionVariableAppend (v, exp) ->
    Printf.sprintf "%s += %s" v (string_of_expression exp)
  | OptionConfigUse c -> Printf.sprintf "use \"%s\"" (string_of_expression c)
  | OptionIfThenElse (cond, ifthen, ifelse) ->
    Printf.sprintf "if %s then %s%s"
      (string_of_condition cond)
      (string_of_set_option ifthen)
      (match ifelse with None -> ""
                       | Some ifelse -> Printf.sprintf " else %s"
                                          (string_of_set_option ifelse))
  | OptionBlock options ->
    Printf.sprintf "begin\n%s\nend"
      (String.concat "\n" (List.map string_of_set_option options))
      *)
val modname_of_fullname : unit
         val modname_of_fullname : string -> string
         val string_of_package_type : unit
         val string_of_package_type : BuildOCPTypes.package_type -> string
