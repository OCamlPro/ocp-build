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

type arg_spec =  ?alias:string list -> ?readme:(unit -> string) -> string -> arg

exception InvalidArg of string
exception Usage (* print help and exit with status 0 *)
exception BuildInfo
exception Version

let arg arg_key arg_spec ?(alias=[]) ?readme arg_usage =
  {
    arg_keys = arg_key :: alias;
    arg_spec;
    arg_readme = readme;
    arg_usage;
  }

let unit arg_key f = arg arg_key (Arg.Unit f)
let string arg_key f = arg arg_key (Arg.String f)

let add_args cmd cmd_args =
  cmd.cmd_arg_list <- cmd.cmd_arg_list @ cmd_args;
  List.iter (fun arg ->
      List.iter (fun key ->
          if key <> "" && not (StringMap.mem key cmd.cmd_arg_map) then
            cmd.cmd_arg_map <- StringMap.add key arg cmd.cmd_arg_map
        ) arg.arg_keys
    ) cmd_args

let command ?(args=[]) ?(group = []) ?readme ?action cmd_usage =
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

let arg_help =
  arg "--help" (Arg.Unit (fun () -> raise Usage))
      " Print command help"

let arg_version =
  arg "--version" (Arg.Unit (fun () -> raise Version))
      " Print version"

let arg_build_info =
  arg "--build-info" (Arg.Unit (fun () -> raise BuildInfo))
      " Print build info"

let split_arg_usage usage =
  try
    let len = String.length usage in
    if String.length usage > 0 && usage.[0] = '{' then
      let pos = String.index usage '}' in
      if pos >= len - 1 then raise Not_found;
      String.sub usage 1 (pos-1), String.sub usage (pos+2) (len - pos -2)
    else
      let pos = String.index usage ' ' in
      String.sub usage 0 pos, String.sub usage (pos+1) (len - pos -1)
  with Not_found -> "", usage

let indent = String.make 22 ' '

let print_usage cmd runner i =

  Printf.printf "%s\n\n" cmd.cmd_usage;

  Printf.printf " %s" (Filename.basename Sys.argv.(0));
  for i = 1 to i do
    Printf.printf " %s" Sys.argv.(i);
  done;
  Printf.printf " [OPTIONS]";
  begin match cmd.cmd_action with
  | Some _ -> Printf.printf " [ARGUMENTS]"
  | None -> ()
  end;
  Printf.printf "\n\n";
  begin
    match cmd.cmd_readme with
    | None -> ()
    | Some f ->
       Printf.printf "%s\n\n" (f ())
  end;

  begin
    match cmd.cmd_group with
    | [] -> ()
    | list ->
       Printf.printf
         "SUBCOMMANDS:   (use subcommand with --help for more info)\n";
       List.iter (fun (name, cmd) ->
           Printf.printf "      %s\n" name;
           Printf.printf "         %s\n" cmd.cmd_usage;
           Printf.printf "\n";
         ) list
  end;

  let cmd_args = [] in
  let cmd_args = match runner with
      { version = Some _ } -> cmd_args @ [arg_version]
    | _ -> cmd_args in
  let cmd_args = match runner with
      { build = Some _ } -> cmd_args @ [arg_build_info]
    | _ -> cmd_args in
  let cmd_args = cmd.cmd_arg_list @ cmd_args @ [arg_help] in

  (* Short help *)

  Printf.printf "OPTIONS:";
  let args = ref StringSet.empty in
  List.iter (fun arg ->

      if List.exists (fun key -> not (StringSet.mem key !args)) arg.arg_keys
      then begin

          let (arg_name, arg_usage) = split_arg_usage arg.arg_usage in
          let arg_len = ref 0 in
          List.iter (fun key ->
              if not (StringSet.mem key !args) then begin
                  if key <> "" then
                    args := StringSet.add key !args;
                  arg_len := max !arg_len (String.length key);
                  Printf.printf "\n  %s %s" key arg_name;
                end;
            ) arg.arg_keys;
          let offset = !arg_len + String.length arg_name in
          if String.length arg_usage > 53 then
            Printf.printf "\n%s%s"
                          (String.sub indent 0
                                      (78 - String.length arg_usage))
                          arg_usage
          else
            if offset > 22 then
            Printf.printf "\n%s%s" indent arg_usage
          else
            Printf.printf "%s%s"
                          (String.sub indent 0 (19 - offset))
                          arg_usage;
        end

    ) cmd_args;
  Printf.printf "\n%!";
  ()


let invalid s = raise (InvalidArg s)

let run_action cmd runner i action =
  try
    action ()
  with
  | Usage ->
     print_usage cmd runner i;
     exit 0
  | InvalidArg arg ->
     Printf.eprintf "Error invalid argument: %s\n%!" arg;
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

and run_arg cmd runner i_group anon i argname =
  match
    let argname, argvalue =
      try
        let pos = String.index argname '=' in
        String.sub argname 0 pos,
        Some (String.sub argname (pos+1) (String.length argname - pos -1))
      with Not_found -> argname, None
    in
    try Some (StringMap.find argname cmd.cmd_arg_map, argvalue)
    with Not_found ->
         match argname, runner with
         | "--help", _ -> Some (arg_help, None)
         | "--version", { version = Some _ } -> Some (arg_version, None)
         | "--build-info", { build = Some _  } -> Some (arg_build_info, None)
         | _ -> None
  with

  | None ->
     if String.length argname > 0 && argname.[0] = '-' then
       run_action cmd runner i_group
                  (fun () -> Printf.kprintf invalid "%S"  argname)
     else
     begin
       match cmd.cmd_action with
       | Some _ ->
          let anon = argname :: anon in
          run_next_arg cmd runner i_group anon i
       | None ->
          run_action cmd runner i_group
                     (fun () -> Printf.kprintf invalid "%S"  argname)
     end


  | Some (arg, argvalue) ->
     eval_arg cmd runner i_group anon i argname arg.arg_spec argvalue

and eval_arg cmd runner i_group anon i argname arg_spec argvalue =
  match arg_spec with
  | Arg.Unit f ->
     run_action cmd runner i_group
                (fun () -> match argvalue with
                           | None -> f ()
                           | Some v ->
                              Printf.kprintf invalid
                                             "%S does not accept an argument (%S provided)"
                                             argname v
                );
     run_next_arg cmd runner i_group anon i
  | Arg.String f ->
     begin
       match argvalue with
       | None ->
          let i = i + 1 in
          if Array.length Sys.argv = i then
            run_action cmd runner i_group
                       (fun () ->
                         Printf.kprintf invalid "missing arg after %S" argname);
          run_action cmd runner i_group (fun () -> f (Sys.argv.(i)));
          run_next_arg cmd runner i_group anon i
       | Some v ->
          run_action cmd runner i_group (fun () -> f v);
          run_next_arg cmd runner i_group anon i
     end
  | _ -> assert false (* cannot yet be built *)

and run_next_arg cmd runner i_group anon i =
  let i = i + 1 in
  if Array.length Sys.argv > i then
    let arg = Sys.argv.(i) in
    run_arg cmd runner i_group anon i arg
  else
    match cmd.cmd_action with
    | None -> ()
    | Some action ->
       let anon = List.rev anon in
       run_action cmd runner i_group (fun () -> action anon)

let run ?version ?build cmd =
  run_group cmd { version; build } 1
