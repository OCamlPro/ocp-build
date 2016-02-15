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


open StringCompat
open OcpLang

let cut_last_extension ~basename =
  try
    let pos = String.rindex basename '.' in
    String.before basename pos,
    String.lowercase (String.after basename pos)
  with Not_found -> (basename, "")

let string_of_channel =
  let size_s = 16000 in
  let s = Bytes.create size_s in
  fun ~ic ->
    let b = Buffer.create 1000 in
    let rec iter () =
      let nread = input ic s 0 size_s in
      if nread > 0 then begin
          Buffer.add_subbytes b s 0 nread;
          iter ()
        end
    in
    iter ();
    Buffer.contents b

let string_of_file ~filename =
  let ic = open_in filename in
  try
    let s = string_of_channel ic in
    close_in ic;
    s
  with e ->
      close_in ic;
      raise e

let output_line ~oc ~str =
  output_string oc (str ^ "\n")

(* [line_break] tells whether or not the '\n' characters need to be kept. *)
let lines_of_file ?(line_break=false) ?(discard = fun _ -> false) filename =
  let chan = open_in filename in
  let x = ref [] in
  begin try
    while true do
      let line = input_line chan in
      if not (discard line) then begin
        let l = if line_break then line ^ "\n" else line in
        x := l :: !x
      end;
    done
    with End_of_file -> ()
  end;
  close_in chan;
  List.rev !x

let file_of_lines ~filename lines =
  File.file_of_lines filename lines

let file_of_string ~filename ~str =
  File.file_of_string filename str


(* We want to keep the exact same lines as in the file (for error
   location) but then we want to discard some lines before using the
   lexer. *)
(*
let genlex_of_file ~keywords ?discard ~filename =
  let lines = lines_of_file ~line_break:true filename in
  Genlex.of_lines keywords filename ?discard lines
*)
