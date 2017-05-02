(**************************************************************************)
(*                                                                        *)
(*   Typerex Libraries                                                    *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type arg = {
    arg_keys : string list;
    arg_spec : Arg.spec;
    arg_readme : (unit -> string) option;
    arg_usage : string;
  }
type command = {
    mutable cmd_args : arg list;
    mutable cmd_group : (string * command) list;
    mutable cmd_anon : (string -> unit);
    cmd_readme : (unit -> string) option;
    cmd_usage : string;
    cmd_action : (unit -> unit);
  }

 and anonymous_args =
   | Basic of (string -> unit)
   | Group of (string * command) list

type runner = {
    version: string option;
    build: string option;
  }

let arg arg_key arg_spec ?readme arg_usage =
  {
    arg_keys = [arg_key];
    arg_spec;
    arg_readme = readme;
    arg_usage;
  }

let runner ?version ?build () = { version; build }

let basic cmd_args cmd_anon ?readme cmd_usage cmd_action =
  {
    cmd_args;
    cmd_anon;
    cmd_group = [];
    cmd_readme = readme;
    cmd_usage;
    cmd_action;
  }

let choose_in_group cmd arg = assert false (* TODO *)

let group cmd_args cmd_group ?readme cmd_usage cmd_action =
  let cmd = {
      cmd_args;
      cmd_anon = (fun _ -> ());
      cmd_group;
      cmd_readme = readme;
      cmd_usage;
      cmd_action;
    } in
  cmd.cmd_anon <- choose_in_group cmd;
  cmd

let add_args cmd cmd_args =
  cmd.cmd_args <- cmd.cmd_args @ cmd_args

let add_commands cmd cmd_group =
  cmd.cmd_group <- cmd.cmd_group @ cmd_group



let run cmd runner = assert false (* TODO *)
exception Usage of int (* print help and exit with status *)
