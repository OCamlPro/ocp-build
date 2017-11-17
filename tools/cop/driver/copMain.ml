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

open Ezcmd.Modules

let version = "0.1"

let commands = [
    CopDo.cmd;
  ]

let () =

  Printexc.record_backtrace true;

  try
    Ezcmd.main_with_subcommands
      ~name:"cop"
      ~doc:"Generic Build System"
      ~man:[
        `P "$(i,COMMAND) is a Composable build system"
      ]
      ~version
      (List.map (fun cmd ->
           { cmd with Arg.cmd_args = cmd.Arg.cmd_args @ CopArgs.common_options }
         ) commands)
  with

  | CopWorkspace.NoWorkspace workspace ->
     Printf.eprintf "Error: could not find workspace root (file '%s')\n"
                    workspace;
     Printf.eprintf "Aborting.\n%!";
     exit 2

  | exn ->
     let backtrace = Printexc.get_backtrace () in
     Printf.fprintf stderr "ocp-build: Fatal Exception %s\n%s\n%!"
                    (Printexc.to_string exn) backtrace;
     raise exn
