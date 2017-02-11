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

(* TODO: re-enable parse_check_spaces. It could be moved somewhere else ! *)

let debug_ocpp = try
  ignore (Sys.getenv ("OCPP_DEBUG"));
  Printf.eprintf "OCPP_DEBUG: trigger debugging\n%!";
  true
with Not_found -> false

open Lexing
open Location
open Parser
open Ocpp_types

(* Might conflict with toplevel directives:
   "quit", "directory", "remove_directory", "cd", "load", "load_rec",
   "use", "mod_use", "install_printer", "remove_printer",
   "trace", "untrace", "untrace_all", "print_depth", "print_length",
   "labels", "principal", "rectypes", "warnings", "warn_error"
*)

type macro = {
  macro_name : string;
  macro_tokens : token list;
}

type pp_state = {
  pp_buffer : Buffer.t;
  pp_name : string;
  mutable pp_last_lnum : int;
  mutable pp_last_fname : string;
}

type state =
  | AfterIf of bool
  | AfterElse of bool
  | InIncludedFileAt of
                        Lexing.lexbuf *
                        Parser.token list *
                        Lexing.position
  | BeginPP of (Lexing.lexbuf -> Parser.token)

type stack = (state * bool) list

let token_of_token = function
  | STRING (s,_) -> Ocpp_parser.STRING s
  | _ -> assert false

let lines_of_file filename =
  let ic = open_in filename in
  let lines = ref [] in
  try
    while true do
      lines := (input_line ic) :: !lines
    done;
    assert false
  with End_of_file ->
    close_in ic;
    List.rev !lines

let files = Hashtbl.create 13

let lexbuf_of_file filename =
  let lines =
    try
      lines_of_file filename
    with _e ->
      Printf.eprintf "Error: could not include file %S\n%!" filename;
      exit 2
  in
  let file_content = String.concat "\n" lines in
  let lines = Array.of_list lines in
  let lexbuf = Lexing.from_string file_content in
  Location.init lexbuf filename;
  Hashtbl.add files filename lines;
  lexbuf

let reset () =
  Hashtbl.clear files

let lines_of_file filename =
  Hashtbl.find files filename

let macro_version version =
 [ LIDENT "version" ; LPAREN; Compat.mk_string version; RPAREN]




module StringMap = Map.Make(String)
let env = ref StringMap.empty
let stack = ref ([] : stack)
let keep = ref true
let current_lexbuf = ref (Lexing.from_string "")
let original_lexbuf = ref !current_lexbuf
let _ = Location.init !current_lexbuf "<none>"
let after_eol = ref true
let new_run = ref true
let queued_tokens = ref []



let optionsMap = ref StringMap.empty
let add_option name f =
  optionsMap := StringMap.add name f !optionsMap

let set_option name v =
  (try
     StringMap.find name !optionsMap
   with Not_found ->
     (fun name _ ->
       Printf.eprintf "Warning: #option %S is not available\n%!" name)
  ) name v

let default_macros = ref
    [
      "OCAML_VERSION", Some (macro_version Sys.ocaml_version);

(*
    "OCAMLLIB", STRING Config.standard_library;
    "ARCHITECTURE", STRING (Config.architecture);
    "MODEL", STRING (Config.model);
    "SYSTEM", STRING (Config.system);

(* Beware for cross-compilation *)
    "OS_TYPE", STRING Sys.os_type;
    "WORD_SIZE", INT Sys.word_size;
    "MAX_STRING_LENGTH", INT Sys.max_string_length;
    "MAX_ARRAY_LENGTH", INT Sys.max_array_length;
*)
    ]
let pp_state = ref (None : pp_state option)
(*let preprocessors = ref StringMap.empty *)

let name_of_token token =
  match token with
  | AMPERAMPER -> "AMPERAMPER"
  | AMPERSAND -> "AMPERSAND"
  | AND -> "AND"
  | AS -> "AS"
  | ASSERT -> "ASSERT"
  | BACKQUOTE -> "BACKQUOTE"
  | BANG -> "BANG"
  | BAR -> "BAR"
  | BARBAR -> "BARBAR"
  | BARRBRACKET -> "BARRBRACKET"
  | BEGIN -> "BEGIN"
  | CLASS -> "CLASS"
  | COLON -> "COLON"
  | COLONCOLON -> "COLONCOLON"
  | COLONEQUAL -> "COLONEQUAL"
  | COLONGREATER -> "COLONGREATER"
  | COMMA -> "COMMA"
  | COMMENT (begin_pos, _end_loc) ->
    Printf.sprintf "COMMENT (%S)" begin_pos
  | CONSTRAINT -> "CONSTRAINT"
  | DO -> "DO"
  | DONE -> "DONE"
  | DOT -> "DOT"
  | DOTDOT -> "DOTDOT"
  | DOWNTO -> "DOWNTO"
  | ELSE -> "ELSE"
  | END -> "END"
  | EOF -> "EOF"
  | EQUAL -> "EQUAL"
  | EXCEPTION -> "EXCEPTION"
  | EXTERNAL -> "EXTERNAL"
  | FALSE -> "FALSE"
  | FOR -> "FOR"
  | FUN -> "FUN"
  | FUNCTION -> "FUNCTION"
  | FUNCTOR -> "FUNCTOR"
  | GREATER -> "GREATER"
  | GREATERRBRACE -> "GREATERRBRACE"
  | GREATERRBRACKET -> "GREATERRBRACKET"
  | IF -> "IF"
  | IN -> "IN"
  | INCLUDE -> "INCLUDE"
  | INHERIT -> "INHERIT"
  | INITIALIZER -> "INITIALIZER"
  | LAZY -> "LAZY"
  | LBRACE -> "LBRACE"
  | LBRACELESS -> "LBRACELESS"
  | LBRACKET -> "LBRACKET"
  | LBRACKETBAR -> "LBRACKETBAR"
  | LBRACKETLESS -> "LBRACKETLESS"
  | LBRACKETGREATER -> "LBRACKETGREATER"
  | LESS -> "LESS"
  | LESSMINUS -> "LESSMINUS"
  | LET -> "LET"
  | LPAREN -> "LPAREN"
  | MATCH -> "MATCH"
  | METHOD -> "METHOD"
  | MINUS -> "MINUS"
  | MINUSDOT -> "MINUSDOT"
  | MINUSGREATER -> "MINUSGREATER"
  | MODULE -> "MODULE"
  | MUTABLE -> "MUTABLE"
  | NEW -> "NEW"
  | OBJECT -> "OBJECT"
  | OF -> "OF"
  | OPEN -> "OPEN"
  | OR -> "OR"
  | PLUS -> "PLUS"
  | PLUSDOT -> "PLUSDOT"
  | PRIVATE -> "PRIVATE"
  | QUESTION -> "QUESTION"
  | QUOTE -> "QUOTE"
  | RBRACE -> "RBRACE"
  | RBRACKET -> "RBRACKET"
  | REC -> "REC"
  | RPAREN -> "RPAREN"
  | SEMI -> "SEMI"
  | SEMISEMI -> "SEMISEMI"
  | SIG -> "SIG"
  | STAR -> "STAR"
  | STRUCT -> "STRUCT"
  | THEN -> "THEN"
  | TILDE -> "TILDE"
  | TO -> "TO"
  | TRUE -> "TRUE"
  | TRY -> "TRY"
  | TYPE -> "TYPE"
  | UNDERSCORE -> "UNDERSCORE"
  | VAL -> "VAL"
  | VIRTUAL -> "VIRTUAL"
  | WHEN -> "WHEN"
  | WHILE -> "WHILE"
  | WITH -> "WITH"

 (* tryocaml: syntax extensions *)
(*
  | SHARPJS -> "SHARPJS"
  | BIGINT s -> Printf.sprintf "BIGINT(%s)" s
*)

  | LIDENT string -> Printf.sprintf "LIDENT(%s)" string
  | OPTLABEL(string) -> Printf.sprintf "OPTLABEL(%s)" string

  | LABEL(string) -> Printf.sprintf "LABEL(%s)" string
  | UIDENT string -> Printf.sprintf  "UIDENT(%s)" string
  | CHAR char -> Printf.sprintf "CHAR(%S)" (String.make 1 char)
  | PREFIXOP(string) -> Printf.sprintf "PREFIXOP(%s)" string
  | INFIXOP0(op) -> Printf.sprintf "INFIXOP0(%s)" op
  | INFIXOP1(op) -> Printf.sprintf "INFIXOP1(%s)" op
  | INFIXOP2(op) -> Printf.sprintf "INFIXOP2(%s)" op
  | INFIXOP3(op) -> Printf.sprintf "INFIXOP3(%s)" op
  | INFIXOP4(op) -> Printf.sprintf "INFIXOP4(%s)" op


  | EOL -> "EOL"

  | _ -> Compat.name_of_token token

let string_of_token token =
  match token with
  | AMPERAMPER -> "&&"
  | AMPERSAND -> "&"

  | AND -> "and"
  | AS -> "as"
  | ASSERT -> "assert"
  | BACKQUOTE -> "`"
  | BANG -> "!"
  | BAR -> "|"
  | BARBAR -> "||"
  | BARRBRACKET -> "|]"
  | BEGIN -> "begin"
  | CLASS -> "class"
  | COLON -> ":"
  | COLONCOLON -> "::"
  | COLONEQUAL -> ":="
  | COLONGREATER -> ":>"
  | COMMA -> ","
  | COMMENT (comment, _loc) ->
    Printf.sprintf "COMMENT (%s)" (String.escaped comment)
  | CONSTRAINT -> "constraint"
  | DO -> "do"
  | DONE -> "done"
  | DOT -> "."
  | DOTDOT -> ".."
  | DOWNTO -> "downto"
  | ELSE -> "else"
  | END -> "end"
  | EQUAL -> "="
  | EXCEPTION -> "exception"
  | EXTERNAL -> "external"
  | FALSE -> "false"
  | FOR -> "for"
  | FUN -> "fun"
  | FUNCTION -> "function"
  | FUNCTOR -> "functor"
  | GREATER -> ">"
  | GREATERRBRACE -> ">}"
  | GREATERRBRACKET -> ">]"
  | IF -> "if"
  | IN -> "in"
  | INCLUDE -> "include"
  | INHERIT -> "inherit"
  | INITIALIZER -> "initializer"
  | LAZY -> "lazy"
  | LBRACE -> "{"
  | LBRACELESS -> "{<"
  | LBRACKET -> "["
  | LBRACKETBAR -> "[|"
  | LBRACKETLESS -> "[<"
  | LBRACKETGREATER -> "[>"
  | LESS -> "<"
  | LESSMINUS -> "<-"
  | LET -> "let"
  | LPAREN -> "("
  | MATCH -> "match"
  | METHOD -> "method"
  | MINUS -> "-"
  | MINUSDOT -> "-."
  | MINUSGREATER -> "->"
  | MODULE -> "module"
  | MUTABLE -> "mutable"
  | NEW -> "new"
  | OBJECT -> "object"
  | OF -> "of"
  | OPEN -> "open"
  | OR -> "or"
  | PLUS -> "+"
  | PLUSDOT -> "+."
  | PRIVATE -> "private"
  | QUESTION -> "?"
  | QUOTE -> "'"
  | RBRACE -> "}"
  | RBRACKET -> "]"
  | REC -> "rec"
  | RPAREN -> ")"
  | SEMI -> ";"
  | SEMISEMI -> ";;"
  | SIG -> "sig"
  | STAR -> "*"
  | STRUCT -> "struct"
  | THEN -> "then"
  | TILDE -> "~"
  | TO -> "to"
  | TRUE -> "true"
  | TRY -> "try"
  | TYPE -> "type"
  | UNDERSCORE -> "_"
  | VAL -> "val"
  | VIRTUAL -> "virtual"
  | WHEN -> "when"
  | WHILE -> "while"
  | WITH -> "with"

 (* tryocaml: syntax extensions *)
  (*  | SHARPJS -> "##"
      | BIGINT s -> Printf.sprintf "%sI" s *)

  | LIDENT string -> string
  | OPTLABEL(string) -> Printf.sprintf "?%s:" string
  | LABEL(string) -> Printf.sprintf "~%s:" string
  | UIDENT string -> Printf.sprintf  "%s" string
  | CHAR char -> Printf.sprintf "'%s'" (String.escaped (String.make 1 char))
  | PREFIXOP(string) -> string
  | INFIXOP0(op) -> op
  | INFIXOP1(op) -> op
  | INFIXOP2(op) -> op
  | INFIXOP3(op) -> op
  | INFIXOP4(op) -> op

  | EOL -> "\n"
  | EOF -> "\n"

  | _ -> Compat.string_of_token token

let string_of_directive = function
  | OCPP_INCLUDE -> "OCPP_INCLUDE"
  | OCPP_DEFINE -> "OCPP_DEFINE"
  | OCPP_DEFUN -> "OCPP_DEFUN"
  | OCPP_UNDEF -> "OCPP_UNDEF"
  | OCPP_IFDEF -> "OCPP_IFDEF"
  | OCPP_IFNDEF  -> "OCPP_IFNDEF"
  | OCPP_IF -> "OCPP_IF"
  | OCPP_ELSE -> "OCPP_ELSE"
  | OCPP_ELIF -> "OCPP_ELIF"
  | OCPP_ENDIF -> "OCPP_ENDIF"
  | OCPP_ERROR -> "OCPP_ERROR"
  | OCPP_WARNING -> "OCPP_WARNING"
  | OCPP_OPTION -> "OCPP_OPTION"
(*
  | OCPP_DBL_LBRACE -> "OCPP_DBL_LBRACE"
  | OCPP_DBL_RBRACE -> "OCPP_DBL_RBRACE"
*)

let string_of_tokens tokens =
  String.concat "," (List.map name_of_token tokens)

(* Entry points in the parser *)

let set_ocaml_version version =
  default_macros := !default_macros @ [
      "OCAML_VERSION", Some (macro_version version) ]

let add_macro name macro =
  default_macros := !default_macros @ [ name, Some macro ]

let remove_macro name =
  default_macros := !default_macros @ [ name, None ]

(* This function is called from Lexer.init when the lexer is initialized *)
let init () =
  new_run := true;
  env := StringMap.empty;
  stack := [];
  keep := true;
  after_eol := true;

  List.iter (fun (macro_name, value) ->
      (*      Printf.eprintf "Defining macro %S\n%!" macro_name; *)
      match value with
      | Some value ->
        env := StringMap.add macro_name {
            macro_name = macro_name;
            macro_tokens = value;
          } !env
      | None ->
        env := StringMap.remove macro_name !env
  ) !default_macros

let parse_expr tokens lexbuf =
  let tokens = ref tokens in
  let module P = Ocpp_parser in
  try
    P.ocpp_expr (fun _lexbuf ->
        match !tokens with
          [] -> P.EOF
        | token :: tail -> tokens := tail;
          match token with
          | INT _ -> P.INT (Compat.int_of_token token)
          | DOT -> P.DOT
          | TRUE -> P.TRUE
          | FALSE -> P.FALSE
          | LIDENT s -> P.LIDENT s
          | UIDENT s -> P.UIDENT s
          | LPAREN -> P.LPAREN
          | RPAREN -> P.RPAREN
          | PLUS -> P.PLUS
          | MINUS -> P.MINUS
          | STAR -> P.STAR
          | EQUAL -> P.EQUAL
          | LESS -> P.LESS
          | GREATER -> P.GREATER
          | INFIXOP0 s -> P.INFIXOP0 s
          | BANG -> P.BANG
          | PREFIXOP s  -> P.PREFIXOP s
          | BARBAR -> P.BARBAR
          | AMPERAMPER -> P.AMPERAMPER
          | _ -> token_of_token token
      ) lexbuf
  with _ ->
    Printf.eprintf "Error while parsing directive expression\n%!";
    exit 2

let compare arg1 arg2 =
  match arg1, arg2 with
  | Int n1, Int n2 -> compare n1 n2
  | String n1, String n2 -> compare n1 n2

  | Version v1, Version v2 -> version_compare v1 v2
  | String s1, Version v2 -> version_compare (version_of_string s1) v2
  | Version v1, String s2 -> version_compare v1 (version_of_string s2)

  | Undefined s, _
  | _, Undefined s ->
    Printf.kprintf failwith "ident %S is undefined in comparison" s.txt
  | _ -> failwith "type mismatch in comparison"

let version_of_string s =
  if debug_ocpp then Printf.eprintf "version_of_string %S\n%!" s;
  let v = version_of_string s in
  if debug_ocpp then
    Printf.eprintf "version(%s) -> %s\n%!" s (string_of_version v);
  v


let int_of_string n s =
  try
    int_of_string s
  with e ->
    Printf.eprintf "int_of_string%d[%S] failed\n%!" n s;
    raise e

let rec ocpp_apply action args =
  if debug_ocpp then
    Printf.eprintf "ocpp_apply: action %S\n%!" action;
  match action, args with
  | "version", [ String s ] -> Version (version_of_string s)
  | "version", [ Int n ] -> Version [ VPositive, VInt n ]
  | "version", [ Version v ] -> Version v

  | "major", [ Version ( (VPositive, VInt major) :: _) ] -> Int major
  | "major", [ Version _  ] -> failwith "version has not major"
  | "major", [ String s ] -> ocpp_apply "major"
                               [ Version (version_of_string s) ]

  | "major", [  _  ] -> failwith "arg is not a version for major(_)"

  | "minor", [ Version ( (VPositive, VInt _) ::
                   (VPositive, VInt minor) :: _) ] -> Int minor
  | "minor", [ Version _  ] -> failwith "version has no minor"
  | "minor", [ String s ] -> ocpp_apply "minor"
                               [ Version (version_of_string s) ]
  | "minor", [  _  ] -> failwith "arg is not a version for minor(_)"

  | "defined", [ String s ] ->
    if StringMap.mem s !env then Int 1 else Int 0
  | "defined",  _  ->
    failwith "for defined, an argument string is"

  | "int", [ String s ] -> Int (int_of_string 6 s)
  | "int", [ Int n ] -> Int n
  | "string", [ String s ] -> String s
  | "string", [ Int n ] -> String (string_of_int n)

  | "!", [ Int 0 ] -> Int 1
  | "!", [ Int _ ] -> Int 0

  | "=", [ arg1; arg2 ] ->
    if compare arg1 arg2 = 0 then Int 1 else Int 0
  | "<>", [ arg1; arg2 ] ->
    if compare arg1 arg2 <> 0 then Int 1 else Int 0
  | ">", [ arg1; arg2 ] ->
    if compare arg1 arg2 > 0 then Int 1 else Int 0
  | "<", [ arg1; arg2 ] ->
    if compare arg1 arg2 < 0 then Int 1 else Int 0
  | ">=", [ arg1; arg2 ] ->
    if compare arg1 arg2 >= 0 then Int 1 else Int 0
  | "<=", [ arg1; arg2 ] ->
    if compare arg1 arg2 <= 0 then Int 1 else Int 0


  | "+", [ String s1; String s2 ] ->
    String (s1 ^ s2)
  | "+", [ Int i1; Int i2 ] ->
    Int (i1 + i2)

  | _ -> failwith "bad expression in #if"

(*
let parse_check_spaces = function
  | "0" ->
    (*     Lexer.check_spaces := None *)
()
  | "file" ->
    let count = ref 0 in
    Lexer.check_spaces := Some
      (fun loc msg ->
        let ppf = Format.err_formatter in
        incr count;
        match !count with
        | 1 ->
          Location.print_loc ppf loc;
          Format.fprintf ppf "@.Warning: %s.@." msg;
        | 2 ->
          Format.fprintf ppf "Warning: (other space errors found, not printed in 'file' mode).@."
        | _ -> ()
      );
  | "error" ->
    Lexer.check_spaces := Some
      (fun loc msg ->
        let ppf = Format.err_formatter in
        Location.print_loc ppf loc;
        Format.fprintf ppf "@.Error: %s.@." msg;
        exit 2
      );
  | v ->
    Lexer.check_spaces := Some
      (fun loc msg ->
        let ppf = Format.err_formatter in
        Location.print_loc ppf loc;
        Format.fprintf ppf "@.Warning: %s.@." msg)
*)

let read_token lexer =
  match !queued_tokens with
  | token :: tail ->
    queued_tokens := tail; token
  | [] ->
    let token = lexer !current_lexbuf in
    if debug_ocpp then begin
      Printf.eprintf "read_token[%s]%s[%d] <- lexer: %s\n%!"
        (if !keep then "keep" else "disc")
        (if !after_eol then "[eol]" else "")
        (List.length !stack) (name_of_token token);
    end;
    token

let expand_token token tokens_ref =
  match token with
  | UIDENT s | LIDENT s ->
    (* Don't expand macros in the discarded branch of #if/#ifdef *)
    if !keep then
      try
        let macro = StringMap.find s !env in
        let tokens = macro.macro_tokens in
        begin match tokens with
            [] -> None
          | token :: tokens ->
            tokens_ref := tokens @ !tokens_ref;
            Some token
        end
      with Not_found ->
        Some token
    else Some token
  | _ ->
    Some token

let expand_tokens tokens =
  let before = tokens in
  let tokens = ref tokens in
  let macro_tokens = ref [] in
  while !tokens <> [] do
    match !tokens with
    | [] -> assert false
    | token :: tail ->
      tokens := tail;
      match expand_token token tokens with
      | None -> ()
      | Some token -> macro_tokens := token :: !macro_tokens
  done;
  let after = List.rev !macro_tokens in
  if debug_ocpp then begin
    Printf.eprintf "expand_tokens:\n   %s\n-> %s\n%!"
      (string_of_tokens before)
      (string_of_tokens after)
  end;
  after

let rec get_token lexer =
  let token = read_token lexer in
  match expand_token token queued_tokens with
  | None ->
    get_token lexer
  | Some token -> token

let bool_of_expr = function
  | String s -> Printf.kprintf failwith "bool(%s)" s
  | Int n -> n <> 0
  | Undefined s -> Printf.kprintf failwith "bool(Undefined %S)" s.txt
  | Version _ -> failwith "bool(Version)"

let tokenify_string lexer s =
  let rec iter lexbuf rev =
    let token = lexer lexbuf in
    if debug_ocpp then
      Printf.eprintf "tokenify_string: %s\n%!" (name_of_token token);
    match token with
    | EOF -> List.rev rev
    | token  ->
      iter lexbuf (token :: rev)
  in
  if debug_ocpp then
    Printf.eprintf "Tokenify %S\n%!" s;
  iter (Lexing.from_string s) []



let rec preprocess lexer =
  let token = get_token lexer in
  if debug_ocpp then begin
    Printf.eprintf "preprocess[%s]%s[%d] <- %s\n%!"
      (if !keep then "keep" else "disc")
      (if !after_eol then "[eol]" else "")
      (List.length !stack) (name_of_token token);
  end;
  match token with
  | EOL -> after_eol := true; preprocess lexer

(*
  | OCPP_PP_DIRECTIVE s ->
    if debug_ocpp then
      Printf.eprintf "OCPP_PP_DIRECTIVE %S\n" s;
    let tokens = tokenify_string Lexer.raw_lexer s in
    if debug_ocpp then
      List.iter (fun token ->
        Printf.eprintf "tokenified in ocpp_pp: %s\n%!"
          (string_of_token token)
      ) tokens;
    after_eol := true;
    queued_tokens := tokens @ [EOL];
    preprocess lexer lexbuf

  | OCPP_PP_CONTENT s ->
    if debug_ocpp then
      Printf.eprintf "OCPP_PP_CONTENT %S\n" s;
    if !keep then begin
      match !pp_state with
      | None -> assert false
      | Some ps ->
        let b = ps.pp_buffer in
        let lnum = lexbuf.lex_curr_p.pos_lnum in
        let fname = lexbuf.lex_curr_p.pos_fname in
        (* TODO: optimize to remove useless lines *)
        Printf.bprintf b "# %d %S\n" lnum fname;
        Buffer.add_string b s;
        Buffer.add_char b '\n';
    end;
    preprocess lexer lexbuf
*)
  | EOF ->
    begin
      match !stack with
        [] -> EOF
      | (InIncludedFileAt (old_lexbuf, queue, curr_p), _) :: stack_tail ->
        old_lexbuf.lex_curr_p <- curr_p;
        queued_tokens := queue;
        after_eol := true;
        if debug_ocpp then
          Printf.eprintf "Popping old lexbuf from stack\n%!";
        current_lexbuf := old_lexbuf;
        stack := stack_tail;
        preprocess lexer
      | _ ->
        Printf.kprintf failwith "unclosed #if/#ifdef"
    end
  | _ ->
    if Compat.is_sharp token && !after_eol then begin
      if debug_ocpp then
      Printf.eprintf "SHARP maybe directive...\n%!";
    begin match get_token lexer with
    | INCLUDE ->
      preprocess_directive lexer  OCPP_INCLUDE
    | LIDENT "define" ->
      preprocess_directive lexer  OCPP_DEFINE
    | LIDENT "defun" ->
      preprocess_directive lexer   OCPP_DEFUN
    | LIDENT "undef" ->
      preprocess_directive lexer  OCPP_UNDEF
    | LIDENT "ifdef" ->
      preprocess_directive lexer  OCPP_IFDEF
    | LIDENT "ifndef" ->
      preprocess_directive lexer  OCPP_IFNDEF
    | IF ->
      preprocess_directive lexer  OCPP_IF
    | LIDENT "elif" ->
      preprocess_directive lexer  OCPP_ELIF
    | ELSE ->
      preprocess_directive lexer  OCPP_ELSE
    | LIDENT "endif" ->
      preprocess_directive lexer  OCPP_ENDIF
    | LIDENT "error" ->
      preprocess_directive lexer  OCPP_ERROR
    | LIDENT "warning" ->
      preprocess_directive lexer  OCPP_WARNING
    | LIDENT "option" ->
      preprocess_directive lexer  OCPP_OPTION
    | token ->
      if debug_ocpp then
        Printf.eprintf "oups, not a directive\n%!";
      queued_tokens := token :: !queued_tokens;
      token
    end
    end else begin


    after_eol := false;
    if !keep then begin
      if !stack <> [] then begin
        let orig_lexbuf = !original_lexbuf in
        let new_lexbuf = !current_lexbuf in
        orig_lexbuf.lex_start_p <- new_lexbuf.lex_start_p;
        orig_lexbuf.lex_curr_p <- new_lexbuf.lex_curr_p;
      end;
      token
    end
    else
      preprocess lexer
 end

and preprocess_directive lexer  directive =
  let rec iter lexer  tokens =
    let token =  read_token lexer  in
    match token with
    | EOF | EOL -> queued_tokens := token :: !queued_tokens; List.rev tokens
    | COMMENT _ -> iter lexer  tokens
    | _ -> iter lexer  (token :: tokens)
  in
  let args = iter lexer  [ ] in

  let do_if args =
    let expr = expand_tokens args in
    let expr = parse_expr expr !current_lexbuf in
    if debug_ocpp then Printf.eprintf "do_if: parsed\n%!";
    let rec eval_expr expr =
      match expr.desc with
      | Pexp_constant (String s) -> String s
      | Pexp_constant (Int n) -> Int n
      | Pexp_uident ident -> Undefined ident
      | Pexp_ident( { txt = "ocaml_version" } ) ->
        begin try
                match (StringMap.find "OCAML_VERSION" !env).macro_tokens with
                | [ (STRING _) as token ] ->
                  Version (version_of_string (Compat.get_STRING token))
                | _ -> raise Not_found
          with Not_found ->
            Printf.eprintf "Error: ocaml_version used, but OCAML_VERSION not defined, or not a string\n%!";
            exit 2
        end
      | Pexp_apply( { desc = Pexp_ident( { txt =  action } ) },
                    args ) ->
        begin
          match action, args with
          | "&&", [ left; right ] ->
            let left = eval_expr left in
            if bool_of_expr left then eval_expr right else left
          | "||", [ left; right ] ->
            let left = eval_expr left in
            if not (bool_of_expr left) then eval_expr right else left
          | _ ->
            let args = List.map (fun arg ->
              eval_expr arg) args in
            ocpp_apply action args
        end
      | _ ->
        if debug_ocpp then Printf.eprintf "do_if error\n%!";
        failwith "bad expression in #if"
    in
    let v = eval_expr expr in
    if debug_ocpp then begin Printf.eprintf "Evaluated\n%!" end;
    match v with
    | Int 0 ->
      stack := (AfterIf false, !keep) :: !stack;
      keep := false
    | Int _n ->
      stack := (AfterIf true, !keep) :: !stack;
      keep := true
    | Undefined s ->
      Printf.kprintf failwith "value of #if %S is undefined, integer" s.txt
    | _ ->
      failwith "result of #if expression is not an integer"
  in

  match directive with

  | OCPP_UNDEF ->
    if !keep then begin
      List.iter (fun token ->
        match token with
        | UIDENT uident  ->
          env := StringMap.remove uident !env
        | _ -> failwith "Macro name expected after #undef"
      ) args;
    end;
    preprocess lexer

  | OCPP_DEFINE ->
    if !keep then begin
      match args with
      | UIDENT macro_name :: tokens ->
        let macro_tokens = expand_tokens tokens in
        let macro = {
          macro_name;
          macro_tokens;
        } in
        env := StringMap.add macro_name macro !env;
        preprocess lexer
      | _ ->
        if debug_ocpp then
          Printf.eprintf "Tokens: %s\n%!" (string_of_tokens args);
        raise (Syntaxerr.Error (Syntaxerr.Other (
          Location.curr !current_lexbuf)))
    end else
      preprocess lexer


  | OCPP_OPTION ->
    if !keep then begin
      match args with
      | [ UIDENT name; STRING _ as token ] ->
        set_option name (Compat.get_STRING token)
      | [ UIDENT name; (INT _) as int ] ->
        set_option name (string_of_int (Compat.int_of_token int))
      | [ UIDENT name ] -> set_option name "1"
      | _ -> failwith "Error after #option: should be Option \"value\""
    end;
    preprocess lexer

  | OCPP_IFDEF ->
    if !keep then begin match args with
      [ UIDENT uident ] ->
        if StringMap.mem uident !env then begin
          stack := (AfterIf true, !keep) :: !stack;
          keep := true;
        end else begin
          stack := (AfterIf false, !keep) :: !stack;
          keep := false;
        end
    | _ -> failwith "Macro name expected after #ifdef"
    end else begin
      stack := (AfterIf true, !keep) :: !stack;
      keep := false
    end;
    preprocess lexer

  | OCPP_IFNDEF ->
    if !keep then begin
      match args with
        [ UIDENT uident ] ->
          if not (StringMap.mem uident !env) then begin
            stack := (AfterIf true, !keep) :: !stack;
            keep := true;
          end else begin
            stack := (AfterIf false, !keep) :: !stack;
            keep := false;
          end
      | _ -> failwith "Macro name expected after #ifndef"
    end else begin
      stack := (AfterIf true, !keep) :: !stack;
      keep := false
    end;
    preprocess lexer

  | OCPP_ENDIF ->
    if args <> [] then failwith "junk after #endif";
    begin match !stack with
    | [] -> failwith "#endif without #if/#ifdef"
    | ((AfterIf _, old_keep) | (AfterElse _, old_keep)) :: stack_tail ->
      keep := old_keep;
      stack := stack_tail
    | _ -> failwith "mismatched #endif"
    end;
    preprocess lexer

  | OCPP_INCLUDE ->
    if !keep then begin
      let args = expand_tokens args in
      match args with
        [ STRING _ as token ] ->
          preprocess_include lexer   (Compat.get_STRING token)
      | _ ->
        failwith "invalid arguments to #include"
    end else
      preprocess lexer

  | OCPP_ELSE ->
    if args <> [] then failwith "junk after #else";
    begin match !stack with
    | [] -> failwith "#else without #if/#ifdef"
    | (AfterIf cond, old_keep) :: stack_tail ->
      keep := (not cond) && old_keep;
      stack := (AfterElse !keep, old_keep) :: stack_tail
    | _ ->
      failwith "#else after #else"
    end;
    preprocess lexer

  | OCPP_ELIF ->

    (* OCPP_ELSE *)
    begin match !stack with
    | [] -> failwith "#else without #if/#ifdef"
    | (AfterIf cond, old_keep) :: stack_tail ->
      keep := old_keep;
      stack := stack_tail;

      if (not cond) && old_keep then begin
        do_if args
      end else begin
        stack := (AfterIf true, old_keep) :: !stack;
        keep := false
      end;
      preprocess lexer

    | _ ->
      failwith "#elif after #else"
    end;


  | OCPP_IF ->
    if !keep then begin
      do_if args
    end else begin
      stack := (AfterIf true, !keep) :: !stack;
      keep := false
    end;
    preprocess lexer

  | OCPP_WARNING ->
    if !keep then begin
      match  expand_tokens args with
        [ STRING _ as token ] ->
          let loc = Location.curr !current_lexbuf in
          if debug_ocpp then
            Printf.eprintf "loc_fname = %S\n%!"  loc.loc_start.pos_fname;
          Format.eprintf "%aWarning: %s\n@."
            Location.print loc  (Compat.get_STRING token);
      | _ -> raise Parsing.Parse_error
    end;
    preprocess lexer

  | OCPP_ERROR ->
    if !keep then begin
      match  expand_tokens args with
        [ STRING _ as token ] ->
          let loc = Location.curr !current_lexbuf in
          if debug_ocpp then
            Printf.eprintf "loc_fname = %S\n%!"  loc.loc_start.pos_fname;
          Format.eprintf "%aError: %s\n@." Location.print loc
             (Compat.get_STRING token);
          exit 2
      | _ -> raise Parsing.Parse_error
    end;
    preprocess lexer

(*
  | OCPP_BEGIN_PP ->
    if !keep then begin
      let set_pp_state pp_name =
        stack := (BeginPP lexer, !keep) :: !stack;
        pp_state := Some { pp_buffer = Buffer.create 100; pp_name;
                           pp_last_lnum = -1; pp_last_fname = "";};
        if debug_ocpp then
          Printf.eprintf "#begin_pp %S OK\n%!" pp_name
      in
      match args with
      | [ STRING _ as token ] -> set_pp_state  (Compat.get_STRING token)
      | [ LIDENT name ] ->
        begin try
                set_pp_state (StringMap.find name !preprocessors)
          with Not_found ->
            Printf.kprintf failwith
              "#begin_pp: unknown external preprocessor %S\n" name
        end
      | _ ->
        failwith "after #begin_pp, a string argument is"
    end else begin
      stack := (BeginPP lexer, !keep) :: !stack;
    end
    if !queued_tokens <> [ EOL ] then begin
      failwith "#begin_pp can only appear at end of macro"
    end;
    if lexer == Ocpp_lexer.lexer then begin
      failwith "#begin_pp cannot appear within #begin_pp"
    end;
    preprocess Ocpp_lexer.lexer

  | OCPP_END_PP ->
    if debug_ocpp then
      Printf.eprintf "OCPP_END_PP...\n%!";
    begin match !stack with
    | (BeginPP lexer, _) :: tail ->
      stack := tail;
      if !keep then begin
        match !pp_state with
        | None -> assert false
        | Some ps ->
          let b = ps.pp_buffer in
          let s = Buffer.contents b in
          if debug_ocpp then
            Printf.eprintf "Buffer:\n%s\n" s;
          let parse_intf = !Ocputils.parse_intf in

          let sourcefile = Filename.temp_file "ocpp_in"
            (if parse_intf then ".mli" else ".ml") in
          let oc = open_out sourcefile in
          output_string oc s;
          close_out oc;

          if debug_ocpp then
            Printf.eprintf "sourcefile=%S\n%!" sourcefile;
          let pp = ps.pp_name in
          let tmpfile = Pparse.call_external_preprocessor sourcefile pp in

          let ast_magic = if parse_intf then
              Config.ast_intf_magic_number else
              Config.ast_impl_magic_number in

          let (ic, is_ast_file) = Pparse.open_and_check_magic tmpfile ast_magic
          in
          if is_ast_file then begin
            close_in ic;
            if parse_intf then
              Parser.OCPP_SIGNATURE (Pparse.read_ast ast_magic tmpfile)
            else
              Parser.OCPP_STRUCTURE (Pparse.read_ast ast_magic tmpfile)
          end else begin
            at_exit (fun _ -> Sys.remove tmpfile);
            preprocess_include lexer  tmpfile
          end
      end else
        preprocess lexer
    | _ -> failwith "#end_pp without #begin_pp"
    end;
*)
  | OCPP_DEFUN
    -> assert false

and preprocess_include lexer   filename =
  (*  Printf.eprintf "include %S\n%!" filename; *)
  let filename =
    if Filename.is_implicit filename ||
       Filename.is_relative filename then
      let current_filename =
        (Location.curr !current_lexbuf).loc_start.pos_fname in
      let local_filename = Filename.concat current_filename filename in
      if Sys.file_exists local_filename then
        local_filename
      else
      try
        Misc.find_in_path !Config.load_path filename
      with Not_found ->
        if debug_ocpp then
          Printf.eprintf "OCPP_INCLUDE current = %S\n%!" current_filename;
        let rec find_in_parents dirname =
          let maybe_filename = Filename.concat dirname filename in
          if Sys.file_exists maybe_filename then
            maybe_filename
          else
            let dirname_dirname = Filename.dirname dirname in
            if dirname_dirname = dirname then
              Printf.kprintf failwith
                "Error: #include %S failed: no such file\n%!" filename
            else
              find_in_parents dirname_dirname
        in
        find_in_parents (Filename.dirname  current_filename)
    else
      filename
  in
  let new_lexbuf = lexbuf_of_file filename in
  let old_lexbuf = !current_lexbuf in
  stack := (InIncludedFileAt (old_lexbuf, !queued_tokens, old_lexbuf.lex_curr_p), true) :: !stack;
  queued_tokens := [];

  if debug_ocpp then Printf.eprintf "Pushing new lexbuf on stack\n%!";
  current_lexbuf := new_lexbuf;
  (*        lexbuf.lex_curr_p <- !current_lexbuf . lex_curr_p; *)
  (*  Printf.eprintf "OK included\n%!"; *)
  preprocess lexer

let last_error = ref None

(* This function is called from Lexer.token when a token is required from
   the lexer. *)
let preprocess lexer lexbuf =
  if !new_run then begin
(*    Printf.eprintf "new_run with new lexbuf\n%!"; *)
    current_lexbuf := lexbuf;
    original_lexbuf := lexbuf;
    new_run := false;
  end;
  try
    let token = preprocess lexer  in
    if debug_ocpp then
      Printf.eprintf "preprocess -> %s\n" (name_of_token token);
    token
  with
  | Failure s ->
    Printf.eprintf "Error: %s\n" s;
    raise (Syntaxerr.Error (Syntaxerr.Other (
          Location.curr !current_lexbuf)))
  | e ->
    last_error := Some (Location.curr !current_lexbuf);
    raise e

let registered = ref false
let register () =
  if not !registered then begin
    registered := true;
    if debug_ocpp then
      Printf.eprintf "[ocpp] activated\n%!";
(*    (try
       parse_check_spaces (Sys.getenv "OCPP_CHECK_SPACES")
     with Not_found -> ());
    Ocpstd.Arg.default_arglist :=
      ("-D", Ocpstd.Arg.String (fun s ->
        add_macro s (try [Compat.mk_string (snd (Misc.cut_at s '='))]
          with Not_found -> [])),
       "<macro[=string]> Define a macro for Ocpp") :: !Ocpstd.Arg.default_arglist;

    Ocpstd.Arg.default_arglist :=
      ("-ocpp-pp", Ocpstd.Arg.String (fun s ->
        try
          let (pp_name, pp_cmd) = Misc.cut_at s '=' in
          preprocessors := StringMap.add pp_name pp_cmd !preprocessors
        with Not_found ->
          Misc.fatal_error "-ocpp-pp 'name=cmd': no command provided"
       ),
       "<pp[=command]> Define an external preprocessor for Ocpp") :: !Ocpstd.Arg.default_arglist;
*)
      Lexer.set_preprocessor init preprocess
  end

let _ =
(*
  add_option "OCPP_CHECK_SPACES" (fun name v -> parse_check_spaces v);
  add_option "Warn" (fun name v -> Warnings.parse_options false v);
  add_option "Warn_error" (fun name v -> Warnings.parse_options true v);
*)
  add_macro "Has_ocpp" [ Compat.token_of_int 1 ];
  register ()

let lexbuf () = !current_lexbuf
