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

(*
  This program watches .cmt files generated in a project, and tries
  to generate corresponding .annot files, using the `ocaml_cmt` tool
  installed by OCaml. It can also be used within the OCaml distribution,
  using then `tools/read_cmt`.

  Its main purpose is to be used when other more complex tools, such
  as `merlin` or `ocp-index` are not yet available for that OCaml
  version.
*)

open StringCompat

let quiet = ref false
let verbose = ref false
let project_includes = ref []
let watch_delay = ref 2


(* When running in OCaml distribution, use the following additional
   configuration: *)
let read_cmt_filename = "tools/read_cmt"
let read_cmt_deps = [
  "compilerlibs/ocamlcommon.cma";
  "compilerlibs/ocamlbytecomp.cma";
  "tools/read_cmt.ml";
  "tools/cmt2annot.ml";
]

let ocaml_includes = [
  "boot";
  "utils";
  "parsing";
  "typing";
  "bytecomp";
  "asmcomp";
  "driver";
  "ocamldoc";
  "lex";
]

let pwd = Sys.getcwd ()
let digest_null = Digest.string ""

(* Try to match digests extracted from .cmt files with .ml files found
   by scanning directories. If found, the generated .annot file will
   be copied to each occurrence of a .ml file with the same digest.
   This should help support _build and _obuild directories.
*)
type source_file = {
  file_name : string;
  mutable file_mtime : float;
  mutable file_digest : Digest.t;
}

and digest_info = {
  digest : Digest.t;
  mutable digest_files : source_file StringMap.t;
}

(* [digest -> digest_info], to find sources with the same digest *)
let digests = ref StringMap.empty

(* [filename -> source_file] to find info on source file *)
let source_files = ref StringMap.empty

(* [cmt_filename -> last mtime] used to only test a .cmt file if it
   has changed *)
let cmt_mtimes = ref StringMap.empty

let file_mtime filename =
  try (Unix.stat filename).Unix.st_mtime with _ -> 0.0

let is_build_required target deps =
  let target_mtime =
    try Some (file_mtime target) with _ -> None
  in
  try
    let deps_mtime = List.map file_mtime deps in
    match target_mtime with
    | None -> true
    | Some target_mtime ->
      try
        List.iter (fun dep_mtime ->
          if dep_mtime > target_mtime then raise Exit
        ) deps_mtime;
        false
      with _ -> true
  with _ -> false

let get_digest digest =
  try
    StringMap.find digest !digests
  with Not_found ->
    let d = {
      digest;
      digest_files = StringMap.empty;
    } in
    digests := StringMap.add digest d !digests;
    d

let get_source_file file_name =
  try
    StringMap.find file_name !source_files
  with Not_found ->
    let file = {
      file_name;
      file_mtime = 0.0;
      file_digest = digest_null;
    } in
    source_files := StringMap.add file_name file !source_files;
    file

let check_read_cmt () =
  if is_build_required read_cmt_filename read_cmt_deps then begin
    if !verbose then
      Printf.eprintf "Re-building read_cmt\n%!";
    Unix.chdir "tools";
    let retcode = Sys.command "make read_cmt" in
    if retcode <> 0 then raise Not_found;
    Unix.chdir pwd;
  end


let rec iter_files dir f =
  let files = try Sys.readdir dir with _ -> [||]  in
  Array.iter (fun file ->
    let filename = Filename.concat dir file in
    if try Sys.is_directory filename with _ -> false then
      iter_files filename f
    else
      f filename
  ) files


let dev_null = " > /dev/null 2> /dev/null"

let read_cmt_info read_cmt filename =
  let source_digest = ref None in
  let info_file = Filename.temp_file
    (Filename.basename filename) ".cmtinfo" in
  let cmd = Printf.sprintf
    "%s -info -o %s %s%s"
    read_cmt
    info_file
    filename
    (if !verbose then "" else dev_null)
  in
  if !verbose then Printf.eprintf "calling %s\n%!" cmd;
  let retcode = Sys.command cmd in
  if retcode <> 0 then raise Exit;
  FileString.iter_lines (fun line ->
    match OcpString.split line ' ' with
    | ["source"; "digest:"; digest] ->
      if !verbose then Printf.eprintf "Source digest found: %s\n%!" digest;
      source_digest := Some digest;
    | _ -> ()
  ) info_file;
  Sys.remove info_file;
  match !source_digest with
  | None -> raise Exit
  | Some digest -> OcpDigest.from_hex digest

let check_cmt read_cmt includes cmt_file =
  let mtime = file_mtime cmt_file in
  let previous_mtime =
    try
      StringMap.find cmt_file !cmt_mtimes
    with Not_found -> 0.0
  in
  if previous_mtime < mtime then begin
    cmt_mtimes := StringMap.add cmt_file mtime !cmt_mtimes;
    if not !quiet then Printf.eprintf "%s%!" cmt_file;
    try
      let ml_digest = read_cmt_info read_cmt cmt_file in
      let annot_files =
        try
          let d = get_digest ml_digest in
          let annot_files = ref [] in
          StringMap.iter (fun _ file ->
            let annot_file =
              (Filename.chop_suffix file.file_name ".ml") ^ ".annot"
            in
            annot_files := annot_file :: !annot_files
          ) d.digest_files;
          !annot_files
        with Not_found ->
          [(Filename.chop_suffix cmt_file ".cmt") ^ ".annot"]
      in

      let annot_files =
        let ml_file = (Filename.chop_suffix cmt_file ".cmt") ^ ".ml" in
        let annot_file =
          (Filename.chop_suffix ml_file ".ml") ^ ".annot"
        in
        if Sys.file_exists ml_file && not (List.mem annot_file annot_files)
        then
          annot_file :: annot_files
        else
          annot_files
      in
      if List.exists (fun annot_file ->
        is_build_required annot_file [cmt_file]) annot_files
      then begin
        let includes = Filename.dirname cmt_file :: includes in
        let includes = match includes with
            [] -> ""
          | _ -> "-I " ^ String.concat " -I " includes
        in

        let cmd = Printf.sprintf
          "%s %s -annot %s%s" read_cmt includes cmt_file
          (if !verbose then "" else dev_null)
        in
        if !verbose then Printf.eprintf "calling %s\n%!" cmd;
        let retcode = Sys.command cmd in
        if retcode <> 0 then raise Exit;
        let generated_file = cmt_file ^ ".annot" in
        let { AnnotParser.annot_filenames } =
          AnnotParser.parse_file generated_file in
        let annot_files = List.fold_left (fun annot_files filename ->
          if !verbose then
            Printf.eprintf "(loc %S)%!" filename;
          if Sys.file_exists filename then
            let annot_file = (Filename.chop_extension filename) ^ ".annot" in
            if not (List.mem annot_file annot_files) then
              let ml_time = file_mtime filename in
              let annot_time = file_mtime annot_file in
              if annot_time < ml_time then
                annot_file :: annot_files
              else annot_files
            else annot_files
          else annot_files
        ) annot_files annot_filenames
        in
        if not !quiet then Printf.eprintf " UPDATED\n%!";
        match annot_files with
          [ annot_file ] ->
            (try Sys.remove annot_file with _ -> ());
            Sys.rename generated_file annot_file;
            if not !quiet then Printf.eprintf "   -> %s\n%!" annot_file
        | [] -> assert false
        | _ ->
          let annot_content = FileString.read_file generated_file in
          Sys.remove generated_file;
          List.iter (fun annot_file ->
            FileString.write_file annot_file annot_content;
            if not !quiet then Printf.eprintf "   -> %s\n%!" annot_file
          ) annot_files
      end else
        if not !quiet then Printf.eprintf " OK\n%!"
    with exn ->
      if !verbose then begin
        Printf.eprintf "\bException %s\n%!" (Printexc.to_string exn)
      end else
        if not !quiet then
          Printf.eprintf " FAILED\n%!"
  end

let check_ml file_name =
  let file = get_source_file file_name in
  let mtime = file_mtime file_name in
  if mtime > file.file_mtime then
    try
      let digest = Digest.file file_name in
      file.file_mtime <- mtime;
      if digest <> file.file_digest then begin
        if file.file_digest != digest_null then begin
          let d = get_digest file.file_digest in
          d.digest_files <- StringMap.remove file_name d.digest_files;
        end;
        file.file_digest <- digest;
        let d = get_digest digest in
        d.digest_files <- StringMap.add digest file d.digest_files
      end;
    with _ ->
      ()

let arg_list = [

  "-I", Arg.String (fun s ->
    project_includes := s :: !project_includes),
  "DIR Read .cmi files from here";

  "--clean", Arg.Unit (fun () ->
    iter_files "." (fun filename ->
      if Filename.check_suffix filename ".annot" then
        try Sys.remove filename with _ ->
          Printf.eprintf "Error: cannot remove %S\n%!" filename
    );
  ), " Clear all .annot files";

  "--verbose", Arg.Set verbose,
  " Verbose mode";
  "--quiet", Arg.Set quiet,
  " Quiet mode (equivalent to -q)";
  "-q", Arg.Set quiet,
  " Quiet mode (equivalent to --quiet)";
  "--delay", Arg.Int ((:=) watch_delay),
  Printf.sprintf "SECS Delay in seconds between scans (default %d)"
    !watch_delay;
]

let () =
  let arg_usage = "ocp-watch-cmt [OPTIONS]" in
  let arg_list = Arg.align arg_list in

  Arg.parse arg_list (fun s ->
    Printf.eprintf "Error: unexpected argument %S\n%!" s;
    Arg.usage arg_list arg_usage;
    exit 2) arg_usage;

  let ocaml_mode =
    try
      List.iter (fun file ->
        if not (Sys.file_exists file) then raise Not_found;
      ) [
        "boot";
        "utils";
        "tools/cmt2annot.ml";
      ];
      Printf.eprintf "Running within OCaml distribution\n%!";
      true
    with Not_found ->
      false
  in

  let includes =
    let project_includes = List.rev !project_includes in
    if ocaml_mode then
      project_includes @ ocaml_includes
    else
      project_includes
  in
  while true do
    begin
      try
        let read_cmt =
          if ocaml_mode then begin
            check_read_cmt ();
            if not (Sys.file_exists read_cmt_filename) then raise Not_found;
            "./boot/ocamlrun ./tools/read_cmt"
          end else
            "ocaml_cmt"
        in
        let cmt_files = ref [] in
        let ml_files = ref [] in
        iter_files "." (fun filename ->
          if Filename.check_suffix filename ".cmt" then
            cmt_files := filename :: !cmt_files
          else
            if Filename.check_suffix filename ".ml" then
              ml_files := filename :: !ml_files
        );
        List.iter check_ml !ml_files;
        List.iter (check_cmt read_cmt includes) !cmt_files;
      with Not_found -> ()
    end;
    Unix.sleep !watch_delay;
  done
