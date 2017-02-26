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
open SimpleConfig.Op (* !! and =:= *)
open AutoconfArgs




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
  Arg.parse AutoconfArgs.arg_list AutoconfArgs.arg_anon AutoconfArgs.arg_usage;

  AutoconfGlobalConfig.load ();
  AutoconfProjectConfig.load ();

  if !arg_git_add then begin
    if not (Sys.file_exists ".git") then
      AutoconfCommon.command "git init"
  end;

  apply_makers ();

  FileString.safe_mkdir ocp_autoconf_dir;
  if Sys.file_exists commit_filename_old then
    Sys.rename  commit_filename_old commit_filename;
  let files = AutoconfFS.commit commit_filename in

  if !arg_git_add then begin
    let cmd = Printf.sprintf "git add %s" (String.concat " " files) in
    AutoconfCommon.command cmd
  end;
  ()
