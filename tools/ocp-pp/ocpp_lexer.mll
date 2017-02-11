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

{
  open Lexing
  open Parser (* why ?? *)

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }
;;

}

let newline = ('\010' | "\013\010" )

rule lexer = parse
    | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
        ("\"" ([^ '\010' '\013' '"' ] * as name) "\"")?
        [^ '\010' '\013'] * newline
        {
          update_loc lexbuf name (int_of_string num) true 0;
          lexer lexbuf
        }
    | '#' [^ '\n']+ { OCPP_PP_DIRECTIVE (Lexing.lexeme lexbuf) }
    | [^ '\n']+     { OCPP_PP_CONTENT (Lexing.lexeme lexbuf) }
    | newline { update_loc lexbuf None 1 false 0; lexer lexbuf }
    | eof { failwith "EOF in Ocpp_lexer.lexer" }
    | _ { failwith "#begin_pp not supported" }
