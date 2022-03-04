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

(* ocp-build prefs [OPTIONS]

  Set the options of the user preference file.

*)

open Ezcmd.V2
open EZCMD.TYPES

open BuildArgs
open BuildOptions

let filename = ref UserOptions.default_filename
let arg_list =
  ("-f", Arg.String (fun s -> filename := s),
   "FILENAME Save preferences to file FILENAME") ::
    [
      arg_set_int UserOptions.njobs_option;
      arg_set_int UserOptions.verbosity_option;

      arg_set_true UserOptions.autoscan_option;
      arg_set_false UserOptions.autoscan_option;

      arg_set_true UserOptions.color_option;
      arg_set_false UserOptions.color_option;

      arg_set_true UserOptions.digest_option;
      arg_set_false UserOptions.digest_option;

      arg_set_true UserOptions.bytecode_option;
      arg_set_false UserOptions.bytecode_option;

      arg_set_true UserOptions.native_option;
      arg_set_false UserOptions.native_option;
    ]

let action () =
  BuildOptions.load_config UserOptions.config_file (FileGen.of_string !filename);
  BuildOptions.apply_arguments ();
  BuildOptions.save_config UserOptions.config_file

let subcommand = EZCMD.sub "prefs"
    ~man: [`P "Set the user global preferences."]
    ~args: ( EZCMD.translate arg_list )
    ~doc: "Set the user global preferences."
    action
