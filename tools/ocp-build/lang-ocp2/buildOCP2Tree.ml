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

type location = {
  loc_begin : Lexing.position;
  loc_end : Lexing.position;
}

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

and expression = {
  exp_expr : expression_expr;
  exp_loc : location;
}

and expression_expr =
  | ExprIdent of string
  | ExprField of expression * string
  | ExprCall of expression * expression list
  | ExprFunction of string list * statement
  | ExprRecord of (string * expression) list
  | ExprList of expression list
  | ExprTuple of expression list
  | ExprValue of BuildValue.Types.value
