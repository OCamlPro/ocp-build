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

type token =
| STRING of string
| IDENT of string
| LPAREN
| RPAREN
| PLUSEQUAL
| EQUAL
| MINUS
| EOF

exception Error

val token : Lexing.lexbuf -> token
