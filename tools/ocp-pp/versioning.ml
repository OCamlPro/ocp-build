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

type version = (version_sign * version_item) list

and version_item =
  | VInt of int
  | VString of string

and version_sign =
  | VPositive
  | VNegative

let rec version_compare v1 v2 =
  match v1, v2 with
  | hd1 :: tl1, hd2 :: tl2 when hd1 = hd2 -> version_compare tl1 tl2
  | [], [] -> 0
  | [], (VPositive, _) :: _ -> -1
  | [], (VNegative, _) :: _ -> 1
  | (VPositive, _) :: _, [] -> 1
  | (VNegative, _) :: _, [] -> -1
  | (VPositive, _) :: _, (VNegative, _) :: _ -> 1
  | (VNegative, _) :: _, (VPositive, _) :: _ -> -1
  | (VPositive, vv1) :: _, (VPositive, vv2) :: _ -> compare vv1 vv2
  | (VNegative, vv1) :: _, (VNegative, vv2) :: _ -> compare vv1 vv2


let int_of_string n s =
  try
    int_of_string s
  with e ->
    Printf.eprintf "int_of_string%d[%S] failed\n%!" n s;
    raise e

let version_of_string s =
  let len = String.length s in
  let rec iter0 s i len =
    if i = len then []
    else
      let c = s.[i] in
      match c with
      | '.' | ' ' -> (VPositive, VString "") :: iter0 s (i+1) len
      | '0'..'9'  -> iter1 s i (i+1) len VPositive
      | '~'       -> iter2 s (i+1) len
      | _         -> iter3 s i (i+1) len VPositive

  and iter1 s i0 i len sign =
    if i = len then
      [ sign, VInt (int_of_string 1 (String.sub s i0 (i-i0))) ]
    else
      let c = s.[i] in
      match c with
      | '.' | ' ' ->
        ( sign, VInt (int_of_string 2 (String.sub s i0 (i-i0))) ) ::
          iter0 s (i+1) len
      | '0' ..'9' ->
        iter1 s i0 (i+1) len sign
      | '~' ->
        ( sign, VInt (int_of_string 3 (String.sub s i0 (i-i0))) ) ::
          iter2 s (i+1) len
      | 'a'..'z' | 'A'..'Z' ->
        ( sign, VInt (int_of_string 4 (String.sub s i0 (i-i0))) ) ::
          iter3 s i (i+1) len sign
      | _ ->
        ( sign, VInt (int_of_string 5 (String.sub s i0 (i-i0))) ) ::
          iter3 s i (i+1) len VPositive

  and iter2 s i len =
    if i = len then
      [ VNegative, VString "" ]
    else
      let c = s.[i] in
      match c with
      | '.' | ' ' ->
        ( VNegative, VString "" ) :: iter0 s (i+1) len
      | '0' ..'9'  ->
        iter1 s i (i+1) len VNegative
      | '~' ->
        ( VNegative, VString "" ) :: iter2 s (i+1) len
      | 'a'..'z' | 'A'..'Z' ->
          iter3 s i (i+1) len VNegative
      | _ ->
        ( VNegative, VString "" ) :: iter3 s i (i+1) len VPositive

  and iter3 s i0 i len sign =
    if i = len then
      [ sign, VString (String.sub s i0 (i-i0)) ]
    else
      let c = s.[i] in
      match c with
      | '.' | ' ' ->
        ( sign, VString (String.sub s i0 (i-i0)) ) ::
          iter0 s (i+1) len
      | '0' ..'9'
        ->
        ( sign, VString (String.sub s i0 (i-i0)) ) ::
          iter1 s i (i+1) len sign
      | '~' ->
        ( sign, VString (String.sub s i0 (i-i0)) ) ::
          iter2 s (i+1) len
      | 'a'..'z' | 'A'..'Z' ->
          iter3 s i0 (i+1) len sign
      | _ ->
        ( sign, VString (String.sub s i0 (i-i0)) ) ::
          iter3 s i (i+1) len VPositive

  in
  iter0 s 0 len

let rec string_of_version  = function
    [] -> ""
  | (sign, VInt n) :: tail ->
    Printf.sprintf "%c%d%s"
      (match sign with VPositive -> '+' | VNegative -> '-')
      n (string_of_version tail)
  | (sign, VString s) :: tail ->
    Printf.sprintf "%c[%s]%s"
      (match sign with VPositive -> '+' | VNegative -> '-')
      s (string_of_version tail)
