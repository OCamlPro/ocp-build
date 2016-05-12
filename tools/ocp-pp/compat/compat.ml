(**************************************************************************)
(*                                                                        *)
(*                              OCamlPro TypeRex                          *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*      (GNU Public Licence version 3.0).                                 *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
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
  | _  -> assert false

let string_of_token = function
  | LBRACKETPERCENT|LBRACKETPERCENTPERCENT
  | LBRACKETAT|LBRACKETATAT|LBRACKETATATAT|PERCENT|PLUSEQ -> "4.02.1 token"
  | STRING (s,_) -> Printf.sprintf "%S" s
  | _  -> assert false

let int_of_token = function
  | INT n -> n
  | _ -> assert false

let token_of_int n = INT n

let loc_of_token lexbuf token =
  match token with
  | COMMENT (_, loc) -> loc
  | _ -> Location.curr lexbuf
