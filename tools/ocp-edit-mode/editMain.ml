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

(* When loading a file, Emacs should call "ocp-edit-mode config
   FILENAME.ml", and this command should return a list of commands to
   execute in return, that will be used to configure the file. *)

let arg_list = [
  "-save-global", Arg.Unit EditOptions.save, " : save global config";
]

let _ =

  EditOptions.load_or_create ();
  EditOptions.maybe_save ();

  Subcommands.parse arg_list
    [
      "install", EditInstall.subcmd_init, EditInstall.subcmd_spec, EditInstall.subcmd_main;
      "config", EditConfig.subcmd_init, EditConfig.subcmd_spec, EditConfig.subcmd_main;
      "emacs", EditEmacs.subcmd_init, EditEmacs.subcmd_spec, EditEmacs.subcmd_main;
      (*      "candidates", EditCandidates.subcmd_init, EditCandidates.subcmd_spec, EditCandidates.subcmd_main; *)
      "documentation", EditDocumentation.subcmd_init, EditDocumentation.subcmd_spec, EditDocumentation.subcmd_main;
      "query", EditQuery.subcmd_init, EditQuery.subcmd_spec, EditQuery.subcmd_main;

    ]
    (String.concat "\n"
       ["ocp-edit-mode SUB-COMMAND [OPTIONS] ARGUMENTS";
        "Configure editors to edit OCaml code";
        "where SUB-COMMAND:";
        ""
       ]
    )
