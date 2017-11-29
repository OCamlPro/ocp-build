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

{
  open OcpCompat

type token =
    Kwd of string
  | Ident of string
  | Int of int
  | String of string


(* the PUSH_LEXERS/POP_LEXER system is used only to deal with
   simple-quoted lists (i.e. quotations and anti-quotations).  *)
type lexer = Lexing.lexbuf -> token list * op option
and op =
  | PUSH_LEXERS of lexer list
  | POP_LEXER

type error =
  | Illegal_character of char
  | Unterminated_comment
  | Unterminated_string
  | Unterminated_string_in_comment
;;

exception Error of error * int * int

open Format

let report_error ppf = function
  | Illegal_character c ->
      fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Unterminated_comment ->
      fprintf ppf "Comment not terminated"
  | Unterminated_string ->
      fprintf ppf "String literal not terminated"
  | Unterminated_string_in_comment ->
      fprintf ppf "This comment contains an unterminated string literal"
;;

module Make(S : sig

end) = struct

(* The table of keywords *)
let keyword_table = Hashtbl.create 149

let add_keyword s = Hashtbl.add keyword_table s (Kwd s)
let find_keyword s =
  try
    Hashtbl.find keyword_table s
  with Not_found -> Ident s

let find_keyword_lexbuf lexbuf =
  find_keyword (Lexing.lexeme lexbuf)

(* To buffer string literals *)

let initial_string_buffer = Bytes.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= Bytes.length (!string_buff) then begin
    let new_buff = Bytes.create (Bytes.length (!string_buff) * 2) in
      Bytes.blit (!string_buff) 0 new_buff 0 (Bytes.length (!string_buff));
      string_buff := new_buff
  end;
  Bytes.unsafe_set (!string_buff) (!string_index) c;
  incr string_index

let get_stored_string () =
  let s = Bytes.sub_string (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  s

(* To translate escape sequences *)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  Char.chr(c land 0xFF)

(* To store the position of the beginning of a string and comment *)
let string_start_pos = ref 0;;
let comment_start_pos = ref [];;

let finish_with tokens lexbuf = tokens, Some POP_LEXER

}

let blank = [' ' '\013' '\009' '\012']
let firstidentchar = ['a'-'z' '\223'-'\246' '\248'-'\255' '_' 'A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let symbolchar2 =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' '<' '=' '>' '?' '@' '^' '|' '~']
(*  ['!' '$' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~'] *)
let decimal_literal = ['0'-'9']+
let hex_literal = '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
let oct_literal = '0' ['o' 'O'] ['0'-'7']+
let bin_literal = '0' ['b' 'B'] ['0'-'1']+
let float_literal =
  ['0'-'9']+ ('.' ['0'-'9']* )? (['e' 'E'] ['+' '-']? ['0'-'9']+)?

rule unquoted = parse

(* BEGIN SHARED PART *)

  | '\n' {
      lexbuf.Lexing.lex_curr_p <- {
        lexbuf.Lexing.lex_curr_p with
        Lexing.pos_lnum = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum + 1 };
      [], None
    }
  | blank +
      { [], None }
  | "\""
      { reset_string_buffer();
        let string_start = Lexing.lexeme_start lexbuf in
        string_start_pos := string_start;
        string lexbuf;
        lexbuf.Lexing.lex_start_pos <-
          string_start - lexbuf.Lexing.lex_abs_pos;
        [String (get_stored_string())], None }
  | "(*"
      { comment_start_pos := [Lexing.lexeme_start lexbuf];
        comment lexbuf;
        [], None }
  | "{"
  | "[" { [find_keyword_lexbuf lexbuf], Some (PUSH_LEXERS [unquoted]) }
  | "}"
  | "]" { [find_keyword_lexbuf lexbuf], Some POP_LEXER }

(* END SHARED PART *)

  | "("
    { [find_keyword_lexbuf lexbuf], Some (PUSH_LEXERS [unquoted]) }
  | ")"
    { [find_keyword_lexbuf lexbuf], Some POP_LEXER }

  | firstidentchar identchar*  {
      [find_keyword_lexbuf lexbuf], None
    }
  | decimal_literal | hex_literal | oct_literal | bin_literal {
      [Int (int_of_string(Lexing.lexeme lexbuf))], None }


  | "#"
  | "&"
  | "&&"
  | "`"
  | "*"
  | "?"
  | "??"
  | "->"
  | "."
  | ".."
  | ":"
  | "::"
  | ":="
  | ":>"
  | ";"
  | ";;"
  | "<"
  | "<-"
  | "="
  | "|"
  | "||"
  | ">"

  | "!="
  | "-"
  | "-."

  | ['!' '~'] symbolchar *

  | '?' symbolchar2 *

  | ['=' '<' '>' '|' '&' '$'] symbolchar *

  | ['@' '^'] symbolchar *

  | ['+' '-'] symbolchar *

  | "**" symbolchar *
  | ","

  | ['*' '/' '%'] symbolchar *
      { [find_keyword_lexbuf  lexbuf], None }



  (* Quoted cases *)

  | "'" [ ^ ' ' '\009' '\010' '\013'  '\012'   '"'  ','  '(' '[' ';' ')' ']' '{' '}' ]+
      { let s = Lexing.lexeme lexbuf in
        let len = String.length s in
        [String (String.sub s 1 (len-1))], None }

  | "'{" { [ find_keyword "function";
             find_keyword "(";
             find_keyword ")";
             find_keyword "{";
             find_keyword "return";
             find_keyword "{";
           ], Some (PUSH_LEXERS [unquoted;
                                 finish_with [
                                     find_keyword ";";
                                     find_keyword "}";
                                   ]
                   ]) }
  | "'(" { [ find_keyword "function";
             find_keyword "(";
             find_keyword ")";
             find_keyword "{";
             find_keyword "return";
             find_keyword "(";
           ], Some (PUSH_LEXERS [unquoted;
                                 finish_with [
                                     find_keyword ";";
                                     find_keyword "}";
                                   ]
                   ]) }

  | "'["
  | "'"
       { [ find_keyword "["], Some (PUSH_LEXERS [quoted]) }

  | eof { [], Some POP_LEXER }
  | _
      { raise (Error(Illegal_character ((Lexing.lexeme lexbuf).[0]),
                     Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)) }

and comment = parse
    "(*"
      { comment_start_pos := Lexing.lexeme_start lexbuf :: !comment_start_pos;
        comment lexbuf;
      }
  | "*)"
      { match !comment_start_pos with
        | [] -> assert false
        | [_] -> ()
        | _ :: l -> comment_start_pos := l;
                    comment lexbuf;
       }
  | "\""
      { reset_string_buffer();
        string_start_pos := Lexing.lexeme_start lexbuf;
        begin try string lexbuf
        with Error (Unterminated_string, _, _) ->
          let st = List.hd !comment_start_pos in
          raise (Error (Unterminated_string_in_comment, st, st + 2))
        end;
        string_buff := initial_string_buffer;
        comment lexbuf }
  | "''"
      { comment lexbuf }
  | "'\\" ['\\' '\'' 'n' 't' 'b' 'r'] "'"
      { comment lexbuf }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { comment lexbuf }
  | eof
      { let st = List.hd !comment_start_pos in
        raise (Error (Unterminated_comment, st, st + 2));
      }
  | _
      { comment lexbuf }

and string = parse
    '"'
      { () }
  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
      { string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r']
      { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char(char_for_decimal_code lexbuf 1);
         string lexbuf }
  | eof
      { raise (Error (Unterminated_string,
                      !string_start_pos, !string_start_pos+1)) }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }

and quoted = parse

(* BEGIN SHARED PART *)

  | '\n' {
      lexbuf.Lexing.lex_curr_p <- {
        lexbuf.Lexing.lex_curr_p with
        Lexing.pos_lnum = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum + 1 };
      [], None
    }
  | blank +
      { [], None }
  | "\""
      { reset_string_buffer();
        let string_start = Lexing.lexeme_start lexbuf in
        string_start_pos := string_start;
        string lexbuf;
        lexbuf.Lexing.lex_start_pos <-
          string_start - lexbuf.Lexing.lex_abs_pos;
        [find_keyword ";"; String (get_stored_string())], None }
  | "(*"
      { comment_start_pos := [Lexing.lexeme_start lexbuf];
        comment lexbuf;
        [], None }

  | "{"
  | "[" { [find_keyword ","; find_keyword_lexbuf lexbuf],
          Some (PUSH_LEXERS [unquoted]) }
  | "}"
  | "]" { [find_keyword_lexbuf lexbuf], Some POP_LEXER }

(* END SHARED PART *)



  | "("
        { [find_keyword ";"; find_keyword_lexbuf lexbuf],
          Some (PUSH_LEXERS [unquoted]) }
  | ")"
  | ","
  | ";"
        { [find_keyword "]"; find_keyword_lexbuf lexbuf], Some POP_LEXER }

  | [ ^ ' ' '\009' '\010' '\013'  '\012' '"'  ',' ';' '{' '(' '[' ']' ')' '}' ]+
      { let s = Lexing.lexeme lexbuf in
        [find_keyword ";"; String s], None }

  | eof { [], Some POP_LEXER }
  | _
      { raise (Error(Illegal_character ((Lexing.lexeme lexbuf).[0]),
                     Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)) }

{
 end

 let make_lexer list =
   let module M = Make(struct end) in
   List.iter (fun kwd -> M.add_keyword kwd) list;

   function () ->
            let queue = ref [] in
            let stack = ref [M.unquoted] in
            let rec iter lexbuf =
              match !queue with
              | token :: tail ->
                 queue := tail;
                 Some token
              | [] ->
                 match !stack with
                 | [] -> None
                 | lexer :: lexers ->
                    let tokens, op = lexer lexbuf in
                    begin
                      match op with
                      | None -> ()
                      | Some (PUSH_LEXERS lexers) ->
                         stack := lexers @ !stack
                      | Some POP_LEXER ->
                         stack := lexers
                    end;
                    match tokens with
                    | [] -> iter lexbuf
                    | [x] -> Some x
                    | list ->
                       queue := list;
                       iter lexbuf


            in
            iter

}
