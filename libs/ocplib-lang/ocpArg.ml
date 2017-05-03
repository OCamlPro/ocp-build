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

(*
TODO:
* print usage, maybe as manpage
* parse arguments' arguments
 *)

open OcpCompat

type arg = {
    arg_keys : string list;
    arg_spec : Arg.spec;
    arg_readme : (unit -> string) option;
    arg_usage : string;
  }

type command = {
    mutable cmd_arg_list : arg list;
    mutable cmd_arg_map : arg StringMap.t;
    mutable cmd_group : (string * command) list;
    cmd_readme : (unit -> string) option;
    cmd_usage : string;
    cmd_action : (string list -> unit) option;
  }

type runner = {
    version: string option;
    build: string option;
  }

exception InvalidArg of string
exception Usage (* print help and exit with status 0 *)
exception BuildInfo
exception Version

let arg arg_key arg_spec ?readme arg_usage =
  {
    arg_keys = [arg_key];
    arg_spec;
    arg_readme = readme;
    arg_usage;
  }

let runner ?version ?build () = { version; build }

let add_args cmd cmd_args =
  cmd.cmd_arg_list <- cmd.cmd_arg_list @ cmd_args;
  List.iter (fun arg ->
      List.iter (fun key ->
          cmd.cmd_arg_map <- StringMap.add key arg cmd.cmd_arg_map
        ) arg.arg_keys
    ) cmd_args

let basic ?(args=[]) ?(group = []) ?readme ?action cmd_usage =
  let cmd = {
      cmd_arg_list = [] ;
      cmd_arg_map = StringMap.empty;
      cmd_group = group;
      cmd_readme = readme;
      cmd_usage;
      cmd_action = action;
    } in
  add_args cmd args;
  cmd

let add_commands cmd cmd_group =
  cmd.cmd_group <- cmd.cmd_group @ cmd_group

                                     (*
let add_runner_arguments cmd runner =

  let args = [] in

  let args = match runner.version with
    | None -> args
    | Some v ->
       (arg "--version" (Arg.Unit (fun () ->
                             Printf.printf "%s\n%!" v;
                             exit 0))
            " Print version and exit")
       :: args
  in

  let args = match runner.build with
    | None -> args
    | Some v ->
       (arg "--build-info" (Arg.Unit (fun () ->
                             Printf.printf "%s\n%!" v;
                             exit 0)) " Print build info and exit")
       :: args
  in

  cmd.cmd_args @ args @
    [ arg "--help" (Arg.Unit (fun () -> raise Usage))
          " Print command help (first argument)" ]
                                      *)

let print_usage cmd runner i =
  Printf.eprintf "fatal: print_usage...\n%!"; exit 2

let run_action cmd runner i action =
  try
    action ()
  with
  | Usage ->
     print_usage cmd runner i;
     exit 0
  | InvalidArg arg ->
     Printf.eprintf "Error: invalid argument %S\n%!" arg;
     print_usage cmd runner i;
     exit 1
  | BuildInfo ->
     Printf.printf "%s\n%!" (match runner.build with
                             | None -> assert false
                             | Some v -> v);
     exit 0
  | Version ->
     Printf.printf "%s\n%!" (match runner.version with
                             | None -> assert false
                             | Some v -> v);
     exit 0

let arg_help =
  arg "--help" (Arg.Unit (fun () -> raise Usage))
      " Print command help"

let arg_version =
  arg "--version" (Arg.Unit (fun () -> raise Version))
      " Print version"

let arg_build_info =
  arg "--build-info" (Arg.Unit (fun () -> raise BuildInfo))
      " Print build info"

let rec run_group cmd runner i =
  if Array.length Sys.argv <= i then
    run_action cmd runner (i-1) (fun () ->
                 match cmd.cmd_action with
                 | None -> raise Usage
                 | Some action -> action []
               )
  else
    let arg = Sys.argv.(i) in
    let group = try Some (List.assoc arg cmd.cmd_group) with _ -> None in
    match group with
    | Some cmd -> run_group cmd runner (i+1)
    | None ->
       run_next_arg cmd runner (i-1) [] (i-1)

and run_arg cmd runner i_group anon i arg =
  match
    try Some (StringMap.find arg cmd.cmd_arg_map)
    with Not_found ->
         match arg, runner with
         | "--help", _ -> Some arg_help
         | "--version", { version = Some _ } -> Some arg_version
         | "--build-info", { build = Some _  } -> Some arg_build_info
         | _ -> None
  with

  | None -> begin
      match cmd.cmd_action with
      | Some _ ->
         let anon = arg :: anon in
         run_next_arg cmd runner i_group anon i
      | None ->
         run_action cmd runner i_group
                    (fun () -> raise (InvalidArg arg))
    end

  | Some arg ->
     begin
       assert false; (* TODO *)
       run_next_arg cmd runner i_group anon i
     end

and run_next_arg cmd runner i_group anon i =
  let i = i + 1 in
  if Array.length Sys.argv >= i then
    let arg = Sys.argv.(i) in
    run_arg cmd runner i_group anon i arg
  else
    let anon = List.rev anon in
    match cmd.cmd_action with
    | None -> ()
    | Some action ->
       run_action cmd runner i_group (fun () -> action anon)

let run cmd runner =
  run_group cmd runner 1
