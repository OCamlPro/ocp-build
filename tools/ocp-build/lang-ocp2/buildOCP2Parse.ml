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

open BuildOCP2Lexer
open BuildOCP2Parser

let getenv_flag s =
  try
    ignore (Sys.getenv s); true
  with Not_found -> false

let flag_print_tokens = getenv_flag "OCPBUILD_PRINT_TOKENS"


let lexer = BuildOCP2Lexer.make_lexer
    [
      ";"; ","; ".";
      "="; "+="; "-=";
      "["; "]"; "("; ")"; "{"; "}";
      "!"; "<"; ">"; ">="; "<="; "&&"; "||";
      "begin"; "end"; "function"; "return";
      "true"; "false"; "if"; "then"; "else";
      "include"; "import";
      "try"; "catch"; "for"; "in";
      "+"; "-"; "*"; "/";
      "'";
    ]

exception ParseError

let read_ocamlconf filename content =
  let lexbuf = Lexing.from_string content in
  lexbuf.Lexing.lex_start_p <-
    { lexbuf.Lexing.lex_start_p with Lexing.pos_fname = filename };
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  if flag_print_tokens then
    Printf.eprintf "Filename: %s\n" filename;
  let token_of_token token_opt =
    if flag_print_tokens then
      Printf.eprintf "%s %!"
                     (match token_opt with
                      | None -> "EOF"
                      | Some token ->
                         match token with
                         | String s -> Printf.sprintf "STRING %S" s
                         | Int _ -> "INT"
                         | Ident s -> s
                         | Kwd s -> s);
    match token_opt with
      None -> EOF
    | Some token ->
      match token with
      | String s -> STRING s
      | Int i -> INT i

      | Kwd "'" -> QUOTE
      | Kwd ";" -> SEMI
      | Kwd "," -> COMMA
      | Kwd "." -> DOT

      | Kwd "=" -> EQUAL
      | Kwd "+=" -> PLUSEQUAL
      | Kwd "-=" -> MINUSEQUAL

      | Kwd "[" -> LBRACKET
      | Kwd "]" -> RBRACKET
      | Kwd "(" -> LPAREN
      | Kwd ")" -> RPAREN
      | Kwd "{" -> LBRACE
      | Kwd "}" -> RBRACE

      | Kwd "!" -> BANG
      | Kwd ">" -> GREATER
      | Kwd ">=" -> GREATEREQUAL
      | Kwd "<" -> LESS
      | Kwd "<=" -> LESSEQUAL
      | Kwd "==" -> EQUALEQUAL
      | Kwd "&&" -> AMPERAMPER
      | Kwd "||" -> BARBAR

      | Kwd "+" -> PLUS
      | Kwd "-" -> MINUS
      | Kwd "*" -> STAR
      | Kwd "/" -> DIVIDE
      | Kwd "%" -> PERCENT

      | Kwd "begin" -> BEGIN
      | Kwd "end" -> END
      | Kwd "function" -> FUNCTION
      | Kwd "return" -> RETURN

      | Kwd "true" -> TRUE
      | Kwd "false" -> FALSE
      | Kwd "if" -> IF
      | Kwd "then" -> THEN
      | Kwd "else" -> ELSE

      | Kwd "include" -> INCLUDE
      | Kwd "import" -> IMPORT

      | Kwd "try" -> TRY
      | Kwd "catch" -> CATCH

      | Kwd "for" -> FOR
      | Kwd "in" -> IN


      | Ident s -> IDENT s
      | Kwd s ->

        Printf.eprintf "Internal error: %S should not be a keyword\n%!" s;
        IDENT s
  in

  let lexer = lexer () in
  let lexer lexbuf =
    try
      token_of_token (lexer lexbuf)
    with BuildOCP2Lexer.Error (error, n, m) ->
      Printf.eprintf "File %S, line 1, characters %d-%d:\n"
        filename n m;
      BuildOCP2Lexer.report_error Format.err_formatter error;
      Format.fprintf Format.err_formatter "@.";
      raise ParseError
  in

  let ast =
    try
      BuildOCP2Parser.main lexer lexbuf
    with Parsing.Parse_error ->
      BuildMisc.print_loc filename (Lexing.lexeme_start lexbuf);
      Printf.eprintf "Parse error\n%!";
      raise ParseError
  in
  ast
