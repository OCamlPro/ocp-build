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

open StdlibArg

(* This is called when the plugin is loaded, or from the main if the plugin
 is linked statically. It is called after the subcommands has been chosen,
   allowing a right choice of arguments. *)

let init subcommand_name =

  let add_args subs args =
    if List.mem subcommand_name subs then
      BuildGlobals.arg_list := !BuildGlobals.arg_list @ args
  in

  add_args ["SUBCOMMAND"; "build"]
    [
      "--print-incomplete-packages", Arg.Set
        BuildOCamlPackage.print_incomplete_packages,
      " Print incomplete packages";

    ];

  ()
