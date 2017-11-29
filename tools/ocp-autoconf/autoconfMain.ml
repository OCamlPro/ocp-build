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

open OcpCompat
open SimpleConfig.Op (* !! and =:= *)
open AutoconfArgs



let show_makers () =
  Printf.printf "Available makers:\n";
  StringMap.iter (fun file _maker ->
      Printf.printf " %s" file
    ) !AutoconfCommon.makers;
  Printf.printf "\n%!"

let arg_list = Arg.align [
    "--save-template", Arg.Set arg_save_template,
    " Save a template if configuration file is not found";
    "--git-add", Arg.Set arg_git_add,
    " Call 'git add' at the end";
    "-f", Arg.Set arg_force,
    " Force overwrite of existing files";
    "--show-makers", Arg.Unit show_makers,
    " Show file makers";
  ]

let arg_usage =
  String.concat "\n" [
    Printf.sprintf "%s [OPTIONS]" (Filename.basename Sys.executable_name);
    "Available options:";
  ]
let arg_anon s =
  Printf.eprintf "Error: unexpected argument %S\n%!" s;
  Arg.usage arg_list arg_usage;
  exit 2


let commit_filename =  Filename.concat autoconf_dir "generated.files"
let commit_filename_old =  "ocp-autoconf.files"

let apply_makers () =
  List.iter (fun file ->
      try
        let maker = StringMap.find file !AutoconfCommon.makers in
        Printf.eprintf "Calling maker for %S\n%!" file;
        maker ();
        AutoconfCommon.makers := StringMap.remove file !AutoconfCommon.makers;
      with Not_found ->
        Printf.eprintf "Warning: no maker found for file %S\n%!" file
    ) !!AutoconfProjectConfig.manage_files;
  Printf.eprintf "Unactive makers:  ";
  StringMap.iter (fun file _ ->
      Printf.eprintf "%s  " file;
    ) !AutoconfCommon.makers;
  Printf.eprintf "\n%!";
  ()

let () =
  Arg.parse arg_list arg_anon arg_usage;

  AutoconfGlobalConfig.load ();
  AutoconfProjectConfig.load ();

  if !arg_git_add then begin
    if not (Sys.file_exists ".git") then
      AutoconfCommon.command "git init"
  end;

  FileString.safe_mkdir ocp_autoconf_dir;
  apply_makers ();

  if Sys.file_exists commit_filename_old then
    Sys.rename  commit_filename_old commit_filename;
  let files = AutoconfFS.commit commit_filename in

  if !arg_git_add then begin
    let cmd = Printf.sprintf "git add %s" (String.concat " " files) in
    AutoconfCommon.command cmd
  end;
  ()
