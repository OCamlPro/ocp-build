(**************************************************************************)
(*                                                                        *)
(*   Typerex Libraries                                                    *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open OcpCompat

(* Message digest (MD5) *)

let _ =
  assert (int_of_char 'a' = 97);
  assert (int_of_char 'A' = 65);
  assert (int_of_char '0' = 48);
  ()

let to_hex_char x =
  if x > 9 then char_of_int (97 - 10 + x) else char_of_int (48 + x)

let of_hex_char c =
  let x = int_of_char c in
  match c with
      'a' .. 'z' -> x - 97 + 10
    | 'A' .. 'Z' -> x - 65 + 10
    | '0' .. '9' -> x - 48
    | _ -> invalid_arg "of_hex_char"

(*let to_hex_old = to_hex *)

let to_hex d =
  let len = String.length d * 2 in
  let result = Bytes.create len in
  let rec iter result d i i2 =
    let c = d.[i] in
    let c = int_of_char c in
    let c1 = c lsr 4 in
    let c2 = c land 15 in
    result.[i2] <- to_hex_char c1;
    result.[i2+1] <- to_hex_char c2;
    let i2 = i2 + 2 in
    if i2 < len-1 then
      iter result d (i+1) i2
  in
  iter result d 0 0;
(*
  if result <> to_hex_old d then begin
    Printf.eprintf "to_hex new = [%s]\n" result;
    Printf.eprintf "to_hex old = [%s]\n" (to_hex_old d);
    exit 2;
  end;
*)
  Bytes.to_string result
;;

let of_hex d =
  let len = String.length d / 2 in
  let result = Bytes.create len in
  let rec iter result d i i2 =
    let c1 = d.[i2] in
    let c2 = d.[i2+1] in
    let c1 = of_hex_char c1 in
    let c2 = of_hex_char c2 in
    result.[i] <- char_of_int ((c1 lsl 4) + c2);
    if i < len-1 then
      iter result d (i+1) (i2+2)
  in
  iter result d 0 0;
(*
  if to_hex_old result <> d then begin
    Printf.eprintf "d = [%s]\n" d;
    Printf.eprintf "r = [%s]\n" (to_hex_old result);
    exit 2;
  end;
*)
  Bytes.to_string result
;;

let from_hex = of_hex


let to_direct_string s = s
let of_direct_string s = s
