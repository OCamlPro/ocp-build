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

let with_location_error ppf f =
  try
    f ()
  with
  | Lexer.Error (err, loc) ->
    Location.print ppf loc;
    Lexer.report_error ppf err;
    Format.fprintf ppf "@.";
    exit 2
  | Syntaxerr.Error err ->
    Syntaxerr.report_error ppf err;
    Format.fprintf ppf "@.";
    exit 2


open Parser

let mk_string s = STRING (s,None)
let get_STRING = function
  | STRING (s,_) -> s
  | _ -> assert false

let name_of_token = function
  | LBRACKETPERCENT|LBRACKETPERCENTPERCENT
  | LBRACKETAT|LBRACKETATAT|LBRACKETATATAT|PERCENT|PLUSEQ -> "4.02.1 token"
  | STRING (s,_) -> Printf.sprintf "STRING(%S,_)" s
  | SHARP -> "SHARP"
  | _  -> assert false

let string_of_token = function
  | LBRACKETPERCENT|LBRACKETPERCENTPERCENT
  | LBRACKETAT|LBRACKETATAT|LBRACKETATATAT|PERCENT|PLUSEQ -> "4.02.1 token"
  | STRING (s,_) -> Printf.sprintf "%S" s
  | SHARP -> "#"
  | _  -> assert false

let is_sharp = function
  | SHARP -> true
  | _ -> false

let int_of_token = function
  | INT n -> n
  | _ -> assert false

let token_of_int n = INT n

let loc_of_token lexbuf token =
  match token with
  | COMMENT (_, loc) -> loc
  | _ -> Location.curr lexbuf
