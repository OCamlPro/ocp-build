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

let debug = ref false

let target_filename = ref None

let arg_list = [
  "-infile", Arg.String (fun file -> target_filename := Some file),
  " FILENAME : where the completion is asked";
  "-debug", Arg.Set debug, " : set debugging mode";
]

let arg_usage = []
let arg_help = []


open Subcommands.TYPES

let subcmd_spec = {
  subcmd_list = arg_list;
  subcmd_usage = arg_usage;
  subcmd_help = arg_help;
}

let subcmd_init () = ()

let functions = [
  "List", [
    "iter"; "map"; "length"; "fold_left"; "fold_right"; "sort";
  ];
  "Printf", [
    "printf"; "sprintf"; "bprintf"; "fprintf"
  ];
  "Array", [
    "create"; "make"; "iter"; "iteri"; "length"; "sort"; "to_list"; "of_list";
  ];
  "String", [
    "sub"; "length"; "create"; "concat";
  ];
  "Buffer", [
    "create"; "contents"; "add_string";
  ];
  "", [ (* Pervasives *)
    "stderr"; "stdout"; "strin"; "compare"; "open_in"; "open_out";
    "input_value"; "output_value";
  ];
  "", [ (* Keywords *)
    "open"; "let"; "for"; "do"; "while"; "to"; "done"; "if"; "then"; "else";
    "include"; "match"; "with"; "begin"; "end"; "raise"; "try";
    "as";
  ];
]

(* Keywords to NOT complete *)
let keywords = [
  "let"; "in";
  "while"; "do"; "done";
  "with";
]

module MLI = struct
  open OCamlApproxLexer

  let tokens_from_file filename =
    let ic = open_in filename in
    try
      let lexbuf = Lexing.from_channel ic in
      let rec iter lexbuf tokens =
        let token = OCamlApproxLexer.token lexbuf in
        match token with
          EOF ->
            close_in ic;
            List.rev tokens
        | _ ->
          iter lexbuf (token :: tokens)
      in
      iter lexbuf []
    with e ->
      close_in ic;
      raise e

  let iter modname f filename =
    let tokens = tokens_from_file filename in
    let rec iter tokens =
      match tokens with
        [] -> ()
      | ( VAL | EXTERNAL ) :: LIDENT lident :: tokens ->
        f (Printf.sprintf "%s%s" modname lident);
        iter tokens
      | _ :: tokens -> iter tokens
    in
    iter tokens

end


let iter_words f filename =
  try
  let s = File.string_of_file filename in
  let len = String.length s in
  let pos = ref (-1) in
  for i = 0 to len-1 do
    match s.[i] with
      'a'..'z' | 'A'..'Z' | '.' | '_' ->
        if !pos < 0 then pos := i
    | _ ->
      if !pos >= 0 then
        f (String.sub s !pos (i- !pos));
      pos := -1
  done;
  if !pos >= 0 then f (String.sub s !pos (len- !pos));
 ()
with e ->
  if !debug then
    Printf.fprintf stderr "Warning: exception %S in iter_words %S\n%!"
      (Printexc.to_string e) filename

let find_in_obuild f prefix =
  let prefix_len = String.length prefix in
  match !target_filename with
    None -> ()
  | Some filename ->
    BuildOCP.find_obuild (fun dir ->
      let completion_dir = Filename.concat dir "_completions" in
      let s = Printf.sprintf "%c.txt" prefix.[0] in
      let filename = Filename.concat completion_dir s in
      if Sys.file_exists filename then
        File.iter_lines (fun line ->
          let (ident, typ) = OcpString.cut_at line ':' in
          let ident_len = String.length ident in
          if ident_len > prefix_len &&
            String.sub ident 0 prefix_len = prefix then
            f ident
        ) filename
    ) (Filename.dirname filename)

let find_candidates prefix =
    let candidates = ref StringSet.empty in
    let prefix_len = String.length prefix in

    List.iter (fun (modname, functions) ->
      let modname =
        if modname <> "" then modname ^ "." else modname in
      let modname_len = String.length modname in
      if modname_len >= prefix_len then begin
        if String.sub modname 0 prefix_len = prefix then
          List.iter (fun f ->
            candidates :=
              StringSet.add (Printf.sprintf "%s%s" modname f) !candidates)
            functions
      end else begin
        if String.sub prefix 0 modname_len = modname then
          List.iter (fun f ->
            let s = Printf.sprintf "%s%s" modname f in
            let s_len = String.length s in
            if s_len >= prefix_len &&
              String.sub s 0 prefix_len = prefix then
              candidates := StringSet.add s !candidates
          ) functions
      end
    ) functions;

    let add_directory dirname =
      try
      Array.iter (fun basename ->
        if Filename.check_suffix basename ".mli" ||
          Filename.check_suffix basename ".ml"
        then
          let modname = Filename.chop_extension basename in
          let modname_len = String.length modname in
          let modname = Printf.sprintf
            "%c%s." (Char.uppercase modname.[0])
            (String.sub modname 1 (modname_len-1)) in
          let modname_len = String.length modname in
          let common_len = min modname_len prefix_len in
          if String.sub modname 0 common_len =
            String.sub prefix 0 common_len then begin
              if prefix_len < modname_len then
                candidates := StringSet.add modname !candidates;
              if Filename.check_suffix basename ".mli" then
              MLI.iter modname (fun s ->
                if String.length s > prefix_len &&
                  String.sub s 0 prefix_len = prefix then
                  candidates := StringSet.add s !candidates
            ) (Filename.concat dirname basename)
            end
      ) (Sys.readdir dirname)
      with _ -> ()
    in
    begin match !target_filename with
      None -> ()
    | Some filename ->
      begin try
        iter_words (fun s ->
          if String.length s > prefix_len &&
             String.sub s 0 prefix_len = prefix then
            candidates := StringSet.add s !candidates
        ) filename;
      with _ -> ()
      end;

      let dirname = Filename.dirname filename in
      add_directory dirname
    end;

    begin try
        let dirname = Sys.getenv "OCP_COMPLETE_DIR" in
        add_directory dirname
      with Not_found -> ()
    end;

    List.iter add_directory !!EditOptions.mli_directories;

    find_in_obuild (fun s -> candidates := StringSet.add s !candidates) prefix;

    let list = ref [] in
    StringSet.iter (fun c -> list := c :: !list) !candidates;
    !list

let find_labels prefix =
(*  Printf.fprintf stderr "find_labels %S\n%!" prefix; *)
    let candidates = ref StringSet.empty in
    let prefix_len = String.length prefix in
    begin match !target_filename with
      None -> ()
    | Some filename ->
      try
              let dirname = Filename.dirname filename in
              Array.iter (fun file ->
(*                Printf.fprintf stderr "test %S\n%!" file; *)
                if Filename.check_suffix file ".ml" then
                  let filename = Filename.concat dirname file in
(*                  Printf.fprintf stderr "in %S\n%!" filename; *)
                  iter_words (fun s ->
                    match s.[0] with
                      'a' .. 'z' -> begin
                    try
                      let pos = String.index s '.' in
                      let s' = String.sub s (pos+1) (String.length s - pos - 1) in
(*                      Printf.fprintf stderr "maybe %S of %S\n%!" s' s; *)
                      let s = s' in
                      if String.length s > prefix_len &&
                        String.sub s 0 prefix_len = prefix then
                        candidates := StringSet.add s !candidates
                    with Not_found -> ()
                      end
                    | _ -> ()
                  ) filename;
              ) (Sys.readdir dirname)
        with e ->
          Printf.fprintf stderr "find_labels: exception %S\n%!" (Printexc.to_string e);

      end;


    let list = ref [] in
    StringSet.iter (fun c -> list := c :: !list) !candidates;
    !list


let subcmd_main args =
  match args with
  | [| prefix |] ->
    let infile = match !target_filename with
      | None -> ""
      | Some filename -> Printf.sprintf " -infile %s" filename in
    Printf.printf "(message \"ocp-edit-mode candidates%s %s\")\n" infile prefix;

    let candidates =
      if List.mem prefix keywords then
        (* do not propose completion when we are writing a keyword *)
        []
      else match prefix.[0] with
      | 'a'..'z' when String.contains prefix '.' ->
        (* this is a label, try another strategy *)
        let pos = String.index prefix '.' in
        let prefix_prefix = String.sub prefix 0 (pos+1) in
        List.map (fun s -> prefix_prefix ^ s)
          (find_labels (String.sub prefix (pos+1)
               (String.length prefix - pos -1)))
      | 'a'..'z'
      | 'A'..'Z' -> find_candidates prefix
        | _ -> []
    in

    let b = Buffer.create 1000 in
    Printf.bprintf b "(setq result (list";
    List.iter (fun s -> Printf.bprintf b " %S" s) candidates;
    Printf.bprintf b "))\n";
    print_string (Buffer.contents b);
    flush stdout

  | _ ->
    Printf.eprintf "usage: %s [-infile <file>] prefix\n" (Filename.basename Sys.argv.(0));
    exit 1
