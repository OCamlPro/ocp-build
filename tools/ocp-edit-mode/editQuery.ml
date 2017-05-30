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

open StringCompat

open SimpleConfig
open Subcommands.TYPES

(*

let target_filename = ref None

let generate_completion dir =
  let completions = Array.make 256 [] in
  let completion_dir = Filename.concat dir "_completions" in
  if not (Sys.file_exists completion_dir) then
    MinUnix.mkdir completion_dir 0o755;

  let rec iter_signature modname sg =
    match sg with
      [] -> ()
    | sig_item :: sg ->
      begin
        match sig_item with
        | Types.Sig_value (ident, { Types.val_type = typ }) ->
          let name = Ident.name ident in
          Buffer.clear Format.stdbuf;
          Printtyp.reset ();
          Printtyp.type_expr Format.str_formatter typ;
          let typ_string = Format.flush_str_formatter () in
          let typ_string = Bytes.of_string typ_string in
          for i = 0 to Bytes.length typ_string - 1 do
            match Bytes.get typ_string i with
              '\n' | '\r' -> typ_string.[i] <- ' '
            | _ -> ()
          done;
          let typ_string = Bytes.to_string typ_string in
          completions.( int_of_char modname.[0] ) <-
            (Printf.sprintf "%s.%s" modname name, typ_string)  ::
          completions.( int_of_char modname.[0] )
        | _ -> ()
      end;
      iter_signature modname sg
  in

  let read_cmis dirname =
    Printf.eprintf "read_cmis %s\n%!" dirname;
    try
      Array.iter (fun basename ->
        if Filename.check_suffix basename ".cmi" then
          let filename = Filename.concat dirname basename in
          try
            let cmi = Cmi_format.read_cmi filename in
            let modname = cmi.Cmi_format.cmi_name in
            iter_signature modname cmi.Cmi_format.cmi_sign
          with e ->
            Printf.eprintf "Exception %S while reading %S\n%!"
              (Printexc.to_string e) filename
      ) (Sys.readdir dirname)
    with e ->
      Printf.eprintf "dir %S: exception %S\n%!" dirname
        (Printexc.to_string e)
  in
  Array.iter (fun s ->
    read_cmis (Filename.concat dir s)) (Sys.readdir dir);
  List.iter read_cmis  !!EditOptions.mli_directories;

  let save_completions filename completions =
    let oc = open_out (Filename.concat completion_dir filename) in
    List.iter (fun (s, stype) ->
      Printf.fprintf oc "%s: %s\n" s stype) completions;
    close_out oc
  in
  Array.iteri (fun i completions ->
    if completions <> [] then
      let filename = Printf.sprintf "%c.txt" (char_of_int i) in
      save_completions filename completions
  ) completions;
  ()

let arg_list = [
  "-infile", Arg.String (fun file -> target_filename := Some file),
  " FILENAME : where the completion is asked";
  "-obuild", Arg.Unit (fun () ->
    BuildOCP.find_obuild generate_completion (Sys.getcwd());
    exit 0
  ), " : read all .cmi files and generate completion/documentation"
]

let find_in_obuild candidate =
  match !target_filename with
    None -> None
  | Some filename ->
    let documentation = ref None in
    BuildOCP.find_obuild (fun dir ->
      let completion_dir = Filename.concat dir "_completions" in
      let s = Printf.sprintf "%c.txt" candidate.[0] in
      let filename = Filename.concat completion_dir s in
      if Sys.file_exists filename then
        FileString.iter_lines (fun line ->
          let (ident, typ) = OcpString.cut_at line ':' in
          if ident = candidate then
            documentation := Some typ
        ) filename
    ) (Filename.dirname filename);
    !documentation

*)

let arg_anon = ref []
let arg_list = [
  "--", Arg.Rest (fun s -> arg_anon := s :: !arg_anon),
  "ARGS Query";
]
let arg_usage = []
let arg_help = []

let subcmd_spec = {
  subcmd_list = arg_list;
  subcmd_usage = arg_usage;
  subcmd_help = arg_help;
}

let subcmd_init () = ()
let subcmd_main (args : string array) =
  let args = List.rev !arg_anon in
  let oc = open_out_gen [Open_creat; Open_append] 0o644
    "/tmp/ocp-edit-mode.log" in
  Printf.fprintf oc "ocp-edit-mode query -- %s\n%!"
    (String.concat " " args);
  close_out oc;
  Printf.eprintf "ocp-edit-mode query -- %s\n%!"
    (String.concat " " args)

    (*
ocp-index print  --show=types   Pervasives.open_out '((:kind . "%k")(:path . "%p")(:type . "%t")(:doc . "%D")(:parent . "%e"))' --separate
((:kind . "val")(:path . "Pervasives.open_out")(:type . "string -> out_channel")(:doc . "Open the named file for writing, and return a new output channel
on that file, positioned at the beginning of the file. The
file is truncated to zero length if it already exists. It
is created if it does not already exists.")(:parent . ""))
    *)
