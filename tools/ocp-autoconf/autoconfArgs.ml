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

open StringCompat

let arg_force = ref false
let arg_git_add = ref false
let arg_save_template = ref false

let arg_list = Arg.align [
    "--save-template", Arg.Set arg_save_template,
    " Save a template if configuration file is not found";
    "--git-add", Arg.Set arg_git_add,
    " Call 'git add' at the end";
    "-f", Arg.Set arg_force,
    " Force overwrite of existing files";
  ]

let arg_usage =
  String.concat "\n" [
    Printf.sprintf "%s [OPTIONS]" (Filename.basename Sys.executable_name);
    "Available options:";
  ]
let arg_anon s =
  Printf.eprintf "Error: unexpected argument %S\n%!" s;
  Arg.usage arg_list arg_usage;
  exit 2
