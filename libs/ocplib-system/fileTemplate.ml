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

include StringTemplate
let create filename labels defaults =
  StringTemplate.create (lazy (FileString.string_of_file filename)) labels defaults

module Set = struct
  open Genlex
  type t = {
    set : StringTemplate.t StringMap.t;
  }
  let lexer = Genlex.make_lexer []
  let lex_line s = OcpGenlex.tokens_of_string_exn lexer s

  exception SyntaxError of int * string
  let error s i = raise (SyntaxError (i,s))

  let of_lines lines =
    let set = ref StringMap.empty in
    let rec iter1 i lines =
      match lines with
        [] -> ()
      | line :: lines ->
        match lex_line line with
        | [ Ident "BEGIN" ; Ident "TEMPLATE" ; String template ] ->
          begin match lines with
              [] -> error "expecting LABELS, found end of file" i
            | line :: lines ->
              match lex_line line with
              | (Ident "LABELS") :: labels ->
                let labels = List.map (function
                      String s -> s
                    | _ -> error "bad label syntax" i) labels in
               iter2 (i+1) (template, labels, Buffer.create 100) lines
              | _ -> error "expecting LABELS" i
          end
        | [] -> iter1 (i+1) lines
        | _ -> error "expecting BEGIN TEMPLATE" i

    and iter2 i ((template, labels, buf) as t) lines =
      match lines with
        [] -> error "end of file before END TEMPLATE" i
      | line :: lines ->
        if OcpString.starts_with line "END " then
          match lex_line line with
          | [ Ident "END"; Ident "TEMPLATE"; String template2 ] ->
            if template <> template2 then error "end of wrong template" i;
            set := StringMap.add template
                (StringTemplate.create (lazy (Buffer.contents buf))
                   labels None) !set;
            iter1 (i+1) lines
          | _ ->
            Buffer.add_string buf line;
            Buffer.add_char buf '\n';
            iter2 (i+1) t lines
        else begin
          Buffer.add_string buf line;
          Buffer.add_char buf '\n';
          iter2 (i+1) t lines
        end

    in
    iter1 1 lines;
    { set = !set }

  let of_string s =
    of_lines (OcpString.split s '\n')

  let of_file filename =
    of_lines ( FileLines.read_file filename )

end

let get set name labs =
  let t = StringMap.find name set.Set.set in
  let labels = labels t in
  List.iter (fun label ->
    if not (List.mem label labels) then
      Printf.kprintf failwith "Template does not provide label %S" label
  ) labs;
  t
