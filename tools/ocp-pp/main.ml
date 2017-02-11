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
open Parser
open Lexing

let debug = ref false

(* [Misc.cut_at] for OCaml >= 4.01 *)
let cut_at s c =
  let pos = String.index s c in
  String.sub s 0 pos, String.sub s (pos+1) (String.length s - pos - 1)



let current_version = "1.0"

let () =

  Ocpp.register ();
  let ppf = Format.err_formatter in

  let prev_line = ref None in

  let preprocess_filename filename =
    Compat.with_location_error ppf (fun () ->
      Location.input_name := filename;

      Ocpp.reset ();
      let lexbuf = Ocpp.lexbuf_of_file filename in
      Lexer.init ();

      let rec iter () =
        match Lexer.token_with_comments lexbuf with
        | EOF ->
          ()
        | token ->

          (* We should error out immediatly if
             (1) a comment is starting on this line and not finishing
             (2) a comment is ending on this line
          *)

          let { Location.loc_start; loc_end } =
            Compat.loc_of_token lexbuf token
          in
          let start_lnum = loc_start.pos_lnum in
          let start_fname = loc_start.pos_fname in

          let curr_lnum = loc_end.pos_lnum in
          let curr_fname = loc_end.pos_fname in

          assert (start_fname = curr_fname);
          if !debug then begin
            Printf.eprintf "TOKEN = %S\n%!" (Ocpp.string_of_token token);
            Printf.eprintf "  lines: %d-%d\n%!" start_lnum curr_lnum;
          end;
          let (lines, prev_lnum) =
            try
              match !prev_line with
              | None ->
                let lines = Ocpp.lines_of_file curr_fname in
                (lines, -10)
              | Some (lines, prev_fname, prev_lnum) ->
                if prev_fname = curr_fname then
                  (lines, prev_lnum)
                else
                  let lines = Ocpp.lines_of_file curr_fname in
                  (lines, -10)
            with Not_found ->
              Printf.eprintf "ocp-pp cannot preprocess a file already preprocessed\n%!";
              exit 2
          in

          let start_lnum =
            if prev_lnum = start_lnum then
              start_lnum+1
            else
              if prev_lnum = start_lnum-1 then
                start_lnum
              else
                if prev_lnum = start_lnum-2 then begin
                  Printf.printf "\n";
                  start_lnum
                end else begin
                  Printf.printf "# %d %S\n" start_lnum curr_fname;
                  start_lnum
                end in

          for num = start_lnum to curr_lnum do
            Printf.printf "%s\n%!" lines.(num-1);
          done;
          prev_line := Some (lines, curr_fname, curr_lnum);

          iter ()
      in
      iter ();
    )
  in
  let arg_usage = "ocpp-pp OPTIONS FILENAMES: preprocess filenames" in
  let arg_list = Arg.align [
    "-D", Arg.String (fun s ->
      try
        let (macro_name, macro_value) = cut_at s '=' in
        Ocpp.add_macro macro_name [Compat.mk_string macro_value]
      with Not_found ->
        Ocpp.add_macro s []),
    "<macro[=string]> Define a macro";
    "-V", Arg.String (fun s ->
      try
        let (macro_name, macro_value) = cut_at s '=' in
        Ocpp.add_macro macro_name (Ocpp.macro_version macro_value)
      with Not_found ->
        Printf.eprintf "Option %S does not define a version\n%!" s;
        exit 1),
    "<macro[=string]> Define a macro";
    "-U", Arg.String (fun s -> Ocpp.remove_macro s),
    "<macro> Undefine a macro";
    "--version", Arg.Unit (fun () ->
      Printf.printf "%s\n%!" current_version;
      exit 0
    ), " Display version";
    "--about", Arg.Unit (fun () ->
      Printf.printf "ocp-pp %s for OCaml %s\n%!"
        current_version Sys.ocaml_version;
      exit 0
      ), " Display general information";
    "--debug", Arg.Set debug,
    " Print tokens and corresponding lines";
    "-I", Arg.String (fun s -> Config.load_path := [s] @ !Config.load_path),
    "<dir> Add directory to path searched for include files";
  ] in
  Arg.parse arg_list preprocess_filename arg_usage;
  ()
