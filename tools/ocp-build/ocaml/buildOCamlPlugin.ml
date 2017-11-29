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

(* This is called when the plugin is loaded, or from the main if the plugin
 is linked statically. It is called after the subcommands has been chosen,
   allowing a right choice of arguments. *)

let init _subcommand_name =

  (*
  let add_args subs args =
    if List.mem subcommand_name subs then
      BuildGlobals.arg_list := !BuildGlobals.arg_list @ args
  in

  add_args ["SUBCOMMAND"; "build"]
    [
      "--print-incomplete-packages", Arg.Set
        BuildOCamlVerifyPackages.print_incomplete_packages,
      " Print incomplete packages";
      "--ocp2-only", Arg.Clear BuildOCP.arg_load_ocp,
      " Do not load .ocp files (no backward compatibility)";
      "--load-ocp", Arg.Set BuildOCP.arg_load_ocp,
      " Load .ocp files (set backward compatibility)";

    ];
   *)

  ()
