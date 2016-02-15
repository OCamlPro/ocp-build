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


(** Extension of the stdlib Genlex module *)

open Genlex

(** Convert a token to a string *)
val string_of_token : token -> string

(*

(** Abstract type for lexers *)
type t

(** [of_lines keywords name lines] generates a lexer from [lines] from
    file [name], using elements of [keywords] as keyword token.
    Optionaly a function can be given to [discard] some lines (usually
    comments) *)
val of_lines : string list -> string -> ?discard:(string -> bool) -> string list  -> t
*)


(** Exception ParseError of char_position * message *)
exception ParseError of int * string

 (** [tokens_of_string lexer string] returns the list of tokens from
     the string using [lexer], and an option with a parse error, if
     one occurred. *)
 val tokens_of_string :
   (char Stream.t -> 'a Stream.t) ->
   string -> 'a list * (int * string) option

 val tokens_of_string_exn :
   (char Stream.t -> 'a Stream.t) ->
   string -> 'a list

(** [tokens_of_string lexer string] returns the list of pairs (token,
    position) from the string using [lexer]. Raise ParseError
    in case of error. *)
val tokenlocs_of_string : (char Stream.t -> token Stream.t) -> string -> (token * int) list
