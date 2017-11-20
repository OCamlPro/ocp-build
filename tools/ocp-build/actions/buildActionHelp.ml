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

open StdlibArg

open OcpCompat
open BuildArgs




let list_ocp_prims () =
  let prims = BuildOCP.primitives_help () in
  Printf.printf "Available primitives: (use -ocp-prim PRIM for details)\n";
  StringMap.iter (fun name help ->
    Printf.printf "%%%s (_) : %s\n" name
      (match help with
         s :: _ -> s
       | [] -> "(no help available)")
  ) prims;
  Printf.printf "%!"

let ocp_prim prim =
  try
    let help = StringMap.find prim (BuildOCP.primitives_help()) in
    Printf.printf "%%%s (ENV) : %s\n%!"
      prim (String.concat "\n   " help)
  with Not_found ->
    Printf.eprintf "No primitive %%%s\n%!" prim;
    BuildMisc.clean_exit 2

let arg_list = [
  "-list-ocp-prims", Arg.Unit list_ocp_prims,
  " Print the list of available primitives(%prim)";

  "-ocp-prim", Arg.String ocp_prim,
   "PRIM Display help on primitive %PRIM";
]

let subcommands = ref ([] : BuildArgs.subcommand list)

let action () =
  ()



let subcommand = {
  sub_name = "help";
  sub_help =  "Help On ocp-build.";
  sub_arg_list = arg_list;
  sub_arg_anon = None;
  sub_arg_usage = [ "Help on ocp-build."; ];
  sub_action = action;
}
