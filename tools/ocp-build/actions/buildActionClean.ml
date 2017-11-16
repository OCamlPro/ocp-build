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

(* ocp-build install [OPTIONS]

  Set the options of the user preference file.

*)

open Ezcmd.Modules
open BuildArgs
open BuildOptions

(* TODO: handle -arch attribute, ie:
   - remove only directories in arch/ subdir
   - don't remove other topdirectories/
*)

let distclean_arg = ref false

let arg_list = [
  "-distclean", Arg.Set distclean_arg, " Remove _obuild directory";
]

let action () =
  let project_root = BuildOptions.find_project_root () in
  let obuild_dir = FileGen.add_basenames project_root
        [ project_build_dirname ] in
  let obuild_dir = FileGen.to_string obuild_dir in
  if !distclean_arg then begin
    Printf.eprintf "Removing _obuild directory\n%!";
    BuildActions.delete_file_or_directory obuild_dir;
  end else begin

    Printf.eprintf "Removing build targets\n%!";


    begin
      try
        let files = Sys.readdir obuild_dir in
        Array.iter (fun file ->
          let filename = Filename.concat obuild_dir file in
          if Sys.is_directory filename then
            BuildActions.delete_file_or_directory filename;
        ) files
      with _ -> ()
    end;
    ()
  end

let subcommand = {
  Arg.cmd_name = "clean";
  cmd_man =  [`P "Clean the project."];
  cmd_args = Arg.translate arg_list;
  cmd_doc = "Clean the project.";
  cmd_action = action;
}
