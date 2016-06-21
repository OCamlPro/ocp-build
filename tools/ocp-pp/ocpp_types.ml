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

open Ocpp_version
open Versioning

type directive =
  | OCPP_INCLUDE
  | OCPP_DEFINE
  | OCPP_DEFUN
  | OCPP_UNDEF
  | OCPP_IFDEF
  | OCPP_IFNDEF
  | OCPP_IF
  | OCPP_ELIF
  | OCPP_ELSE
  | OCPP_ENDIF
  | OCPP_ERROR
  | OCPP_WARNING
  | OCPP_OPTION

type value =
  | Undefined of string Location.loc
  | String of string
  | Int of int
  | Version of version

type expression = { desc : expr_desc; eloc : Location.t }
and expr_desc =
| Pexp_ident of string Location.loc
| Pexp_uident of string Location.loc
| Pexp_constant of  value
| Pexp_apply of expression * expression list
