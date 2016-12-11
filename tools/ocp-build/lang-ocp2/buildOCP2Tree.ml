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

open BuildValue.Types
open StringCompat

exception OCPExn of location * string * BuildValue.Types.value

type statement = {
  stmt_expr : statement_expr;
  stmt_loc : location;
}

and statement_expr =
  | StmtSeq of statement * statement
  | StmtEmpty
  | StmtInclude of expression * statement
  | StmtAssign of expression * expression
  | StmtExpr of expression
  | StmtIfthenelse of expression * statement * statement
  | StmtReturn of expression option
  | StmtBlock of statement
  | StmtImport of expression
  | StmtTry of statement * (string * (string * statement)) list
  | StmtFor of string * expression * statement

and expression = {
  exp_expr : expression_expr;
  exp_loc : location;
}

and expression_expr =
  | ExprIdent of string
  | ExprField of expression * expression
  | ExprCall of expression * expression list
  | ExprFunction of string list * statement
  | ExprRecord of (string * expression) list
  | ExprList of expression list
  | ExprTuple of expression list
  | ExprValue of BuildValue.Types.value
  | ExprTry of expression * (string * (string * expression)) list

(* Primitives directly emitted by the parser for operators *)

let prim_not_name = "not" (* not and neg *)
let prim_or_name = "or"
let prim_and_name = "and"
let prim_xor_name = "xor"

let prim_identity_name = "identity"

let prim_add_name = "add"
let prim_sub_name = "sub"
let prim_mul_name = "mul"
let prim_div_name = "div"
let prim_mod_name = "mod"

let prim_lsl_name = "lsl"
let prim_lsr_name = "lsr"
let prim_lessthan_name = "lessthan"
let prim_greaterthan_name = "greaterthan"
let prim_lessequal_name = "lessequal"
let prim_greaterequal_name = "greaterequal"
let prim_equal_name = "equal"
let prim_notequal_name = "notequal"
