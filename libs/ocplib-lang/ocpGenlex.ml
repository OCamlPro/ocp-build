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

include Genlex

let string_of_token token =
  match token with
  | Ident id -> Printf.sprintf "Ident \"%s\"" id
  | Kwd id   -> Printf.sprintf "Kwd \"%s\"" id
  | Int n    -> Printf.sprintf "Int %d" n
  | Char c   -> Printf.sprintf "Char '%c'" c
  | String s -> Printf.sprintf "String \"%s\"" (String.escaped s)
  | Float n  -> Printf.sprintf "Float %f" n

exception ParseError of int * string

let tokens_of_string lexer s =
  let str1 = Stream.of_string s in
  let str2 = lexer str1 in
  let list = ref [] in
  let error = try
                Stream.iter (fun token ->
                  list := token :: !list) str2;
                None
    with
      Stream.Error error ->
        Some (Stream.count str1, error)
  in
  List.rev !list, error

let tokens_of_string_exn lexer s =
  match tokens_of_string lexer s with
  | tokens, None -> tokens
  | _, Some (pos, error) -> raise (ParseError(pos,error))

let tokenlocs_of_string lexer s =
  let str1 = Stream.of_string s in
  let str2 = lexer str1 in
  let list = ref [] in
  try
    Stream.iter (fun token ->
      let token_pos = Stream.count str1 in
      list := (token, token_pos) :: !list) str2;
    List.rev !list
  with
    Stream.Error error ->
      raise (ParseError (Stream.count str1, error))
