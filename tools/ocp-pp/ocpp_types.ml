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
