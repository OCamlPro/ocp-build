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

let workspace_arg = ref "cop-workspace"
let root_arg = ref None
let local_arg = ref false

let common_options =
  let docs = "COMMON OPTIONS" in
  [
    [ "workspace" ],
    Arg.Set_string workspace_arg,
    Ezcmd.info ~docv:"WORKSPACE" ~docs
               ~env:(Ezcmd.env "COP_WORKSPACE")
               "Set the workspace file to look for ('cop-workspace' by default)";

    ["root" ],
    Arg.File (fun d -> root_arg := Some d),
    Ezcmd.info ~docv:"DIR" ~docs
               ~env:(Ezcmd.env "COP_ROOT")
               "Set the root of the workspace";

    ["local"],
    Arg.Set local_arg,
    Ezcmd.info ~docs
               ~env:(Ezcmd.env "COP_LOCAL")
               "Choose local workspace instead of the largest one";

    ["verbose"],
    Arg.Set_int CopGlobals.verbose,
    Ezcmd.info ~docv:"N" ~docs
               ~env:(Ezcmd.env "COP_VERBOSE")
               "Set the verbosity level"

  ]

let lookup_root () =
  let root =
    CopWorkspace.lookup_root
      ~root: !root_arg
      ~local: !local_arg
      ~workspace: !workspace_arg
  in
  (root, !workspace_arg)
