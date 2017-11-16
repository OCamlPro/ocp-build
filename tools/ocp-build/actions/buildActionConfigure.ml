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

let command_name = "configure"
let command_help = {|
  ocp-build configure [CONFIGURE_OPTIONS]

  Set the configuration options of the project. This is mainly done using
the options `--with FEATURE`, `--with FEATURE=VALUE` and `--without FEATURE`.
|}

open Ezcmd.Modules

open BuildArgs
open BuildOptions

let with_args = ref []

(* BuildOCP2Prims.with_feature *)

let arg_with =
  Arg.translate ~docs:"FEATURES OPTIONS"
  [
  "--with", Arg.String (fun name -> with_args := (name, true) :: !with_args),
  " Enable a feature (X), maybe setting its value (X=Y)";

  "--without", Arg.String (fun name -> with_args := (name, false) :: !with_args),
  " Disable a feature (X)";
  ]

let arg_list =
  Arg.translate ~docs:"CONFIGURE OPTIONS"
  [

  arg_set_int_option ProjectOptions.njobs_option;
  arg_set_int_option ProjectOptions.verbosity_option;

  arg_set_true_option ProjectOptions.autoscan_option;
  arg_set_false_option ProjectOptions.autoscan_option;

  arg_set_true_option ProjectOptions.digest_option;
  arg_set_false_option ProjectOptions.digest_option;

  arg_set_true_option ProjectOptions.bytecode_option;
  arg_set_false_option ProjectOptions.bytecode_option;

  arg_set_true_option ProjectOptions.native_option;
  arg_set_false_option ProjectOptions.native_option;

(* ProjectOptions.meta_dirnames_option *)
  arg_set_string_option ProjectOptions.install_destdir_option;
  arg_set_string_option ProjectOptions.install_bindir_option;
  arg_set_string_option ProjectOptions.install_libdir_option;
  arg_set_string_option ProjectOptions.install_datadir_option;

  arg_set_string_option ProjectOptions.ocamllib_option;

  arg_set_true ProjectOptions.use_ocamlfind_option;
  arg_set_false ProjectOptions.use_ocamlfind_option;

]

(* TODO: save project `with_args` so that it can be reloaded in
    BuildActionCheck *)

let action () =
  let project_root = BuildOptions.find_project_root () in
(*  Printf.eprintf "project_root = %S\n" (FileGen.to_string project_root); *)
  let filename = FileGen.add_basenames project_root
      [ project_build_dirname; project_config_basename ] in

  BuildOptions.load_config ProjectOptions.config_file filename;
  BuildOptions.apply_arguments ();
  BuildOptions.save_config ProjectOptions.config_file

let subcommand = {
  Arg.cmd_name = command_name;
  cmd_man = [ `P command_help ];
  cmd_args = arg_with @ arg_list;
  cmd_doc = "Set the project options.";
  cmd_action = action;
}
