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

let command_name = "init"
let command_help = {|
ocp-build init [INIT_OPTIONS] [CONFIGURE_OPTIONS]

  * creates the _obuild sub-directory in the current directory
  * configures the project according to CONFIGURE_OPTIONS. See
      `ocp-build configure --help` for specific help.
|}

open OcpCompat

open BuildArgs

open SimpleConfig.Op

open BuildEngineTypes
open BuildTerm (* Terminal functions *)
open BuildOptions (* cin_... *)
open BuildOCamlConfig.TYPES
open BuildOCPTypes
open BuildValue.TYPES
open BuildActions
open BuildTypes

open StdlibArg

let _verbose = OcpDebug.verbose_function ["B"; "BuildActionInit"]

let action () =
  BuildActionsWarnings.set_default_is_always ();

  let root_dir = BuildOptions.project_build_dirname in

  if Sys.file_exists root_dir then begin
    if not (Sys.is_directory root_dir) then begin
      Printf.eprintf "Error: cannot create %s/ directory:\n" root_dir;
      Printf.eprintf "  %s exists, but is not a directory\n" root_dir;
      BuildMisc.clean_exit 2
    end;
    (* TODO: we should probably do some check to verify that we have
    write permission everywhere in _obuild. *)
  end else begin
    BuildMisc.safe_mkdir BuildOptions.project_build_dirname
    end;
  BuildActionConfigure.action ()

let arg_list = [
    "", Arg.Unit (fun()->()), "\nList of options available in INIT_OPTIONS:\n" ;

  ]

let subcommand = {
  sub_name = command_name;
  sub_help = command_help;
  sub_arg_list = arg_list
                 @ BuildActionConfigure.arg_with
                 @ BuildActionsWarnings.arg_list;
  sub_arg_anon = None;
  sub_arg_usage = [
    "Set the root of a project.";
  ];
  sub_action = action;
}
