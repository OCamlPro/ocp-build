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

open OcpCompat
open BuildEngineTypes
open BuildEngineGlobals

let verbose = OcpDebug.verbose_function [ "B";"BuildRules"]

(* Rule Misc Functions *)

let new_rule rule_context rule_loc rule_main_target rule_commands =
  let rule_id = new_rule_id rule_context in
  let r = {
    rule_id;
    rule_main_target;
    rule_temp_dir = None;
    rule_commands;
    rule_loc;
    rule_forced = false;
    rule_sources = IntMap.empty;
    rule_time_dependencies = IntMap.empty;
    rule_temporaries = [];
    rule_targets = IntMap.empty;
    rule_missing_sources = 0;
    rule_state = RULE_INACTIVE;

    rule_context;
  } in
  Hashtbl.add rule_context.build_rules r.rule_id r;
  rule_main_target.file_target_of <- r :: rule_main_target.file_target_of;
  r.rule_targets <- IntMap.add rule_main_target.file_id rule_main_target r.rule_targets;
  r

let add_rule_source r file =
  if not (IntMap.mem file.file_id r.rule_sources) then begin
    r.rule_sources <- IntMap.add file.file_id file r.rule_sources;
    file.file_source_for <- r :: file.file_source_for
  end

let add_rule_time_dependency r file =
  if not (IntMap.mem file.file_id r.rule_time_dependencies) then begin
    r.rule_time_dependencies <- IntMap.add file.file_id file r.rule_time_dependencies;
    file.file_source_for <- r :: file.file_source_for
  end

let add_rule_sources r files =
  List.iter (add_rule_source r) files

let add_rule_target r file =
  if not (IntMap.mem file.file_id r.rule_targets) then begin
    r.rule_targets <- IntMap.add file.file_id file r.rule_targets;
    if verbose 4 && file.file_target_of <> [] then
      Printf.eprintf "Warning: file %s targetted by multiple rules\n" (file_filename file);
    file.file_target_of <- r :: file.file_target_of
  end

let add_rule_targets r files =
  List.iter (add_rule_target r) files

let add_rule_temporary r file =
  match file.file_kind with
    FILE_TEMPORARY ->
      r.rule_temporaries <- file :: r.rule_temporaries
  | FILE_VIRTUAL | FILE_REAL ->
    Printf.ksprintf failwith "Temporary file %s is also real" (file_filename file)

let add_rule_command r cmd =
  r.rule_commands <- r.rule_commands @ [cmd]

let add_rule_commands r cmds =
  r.rule_commands <- r.rule_commands @ cmds

let add_rule_temporaries r files =
  List.iter (add_rule_temporary r) files

(* Commands Misc Functions *)

let new_command cmd args = {
  cmd_command = cmd;
  cmd_args = args;
  cmd_stdin_pipe = None;
  cmd_stdout_pipe = None;
  cmd_stderr_pipe = None;
  cmd_move_to_dir = None;
}

let string_of_argument arg =
  match arg with
    S s -> BuildSubst.subst_global s
  | T s -> "${temp}/" ^ BuildSubst.subst_global s
  | F f -> FileGen.to_string f
  | BF f -> FileGen.to_string f.file_file
  | BD d -> d.dir_fullname

let rule_temp_dir r =
  match r.rule_temp_dir with
  | Some dir -> dir
  | None ->
    let hash = Digest.to_hex
        (Digest.string
           (file_filename r.rule_main_target)) in
    let dir =
      FileGen.add_basename r.rule_context.build_dir ("RULE-" ^ hash) in
    r.rule_temp_dir <- Some dir;
    dir

let rule_subst r s =
  BuildSubst.substitute (fun r s ->
    match s with
    | "RULE_TEMP_DIR" -> FileGen.to_string (rule_temp_dir r)
    | _ ->
      try
        StringMap.find s (BuildSubst.global_subst ())
      with Not_found ->
        Printf.sprintf "%%{%s}%%" s) r s

let file_of_argument r arg =
  match arg with
    S s -> FileGen.of_string (rule_subst r s)
  | T s -> FileGen.add_basename (rule_temp_dir r) (rule_subst r s)
  | F f -> f
  | BF f -> f.file_file
  | BD d -> d.dir_file

let argument_of_argument r arg =
  match arg with
    S s -> rule_subst r s
  | T s -> FileGen.to_string (
    FileGen.add_basename (rule_temp_dir r) (rule_subst r s))
  | F f -> FileGen.to_string f
  | BF f -> FileGen.to_string f.file_file
  | BD d -> d.dir_fullname


let command_of_command r cmd =
  List.map (rule_subst r) cmd.cmd_command

let argument_of_string s = S s

let add_command_string cmd arg =
  cmd.cmd_args <- cmd.cmd_args @ [S arg]

let add_command_arg cmd arg =
  cmd.cmd_args <- cmd.cmd_args @ [arg]

let add_command_strings cmd args =
  cmd.cmd_args <- cmd.cmd_args @ (List.map argument_of_string args)

let add_command_args cmd args =
  cmd.cmd_args <- cmd.cmd_args @ args

let add_command_pipe cmd filename =
  cmd.cmd_stdout_pipe <- Some filename
