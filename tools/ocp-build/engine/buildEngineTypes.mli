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

val verbose : int -> bool

type delete_orphans =
    KeepOrphans
  | DeleteOrphanFiles
  | DeleteOrphanFilesAndDirectories

type rule_state =
    RULE_INACTIVE (* A rule is RULE_INACTIVE if it is not needed for the current compilation process. *)
  | RULE_ACTIVE   (* A rule is RULE_ACTIVE if it is needed for the current compilation process. However,
      it will move to RULE_WAITING if it should be evaluated because its chain of dependencies requires it. *)
  | RULE_WAITING  (* A rule is RULE_WAITING if it needs to be evaluated *)
  | RULE_EXECUTING (* A rule is RULE_EXECUTING if it has been extracted from the queues by 'next_rule' and
       not yet finished evaluating. *)
  | RULE_EXECUTED  (* A rule is RULE_EXECUTED if it has been executed. *)

type file_kind =
  | FILE_REAL
  | FILE_VIRTUAL
  | FILE_TEMPORARY


module DigestMap : Map.S with type key = Digest.t

type only_if_changed = bool

type build_rule = {
  rule_id : int;
  (* Two rules should never have the same main target, otherwise their temp
     directories will be the same ! *)
  rule_main_target : build_file;
  mutable rule_temp_dir : FileGen.t option;
  mutable rule_forced : bool;
  mutable rule_commands :  build_action list;
  rule_loc : build_loc; (* project_info *)
  mutable rule_sources :  build_file IntMap.t;

  (* rule_time_dependencies: dependencies that are not required, but if the rules that generate them
     are active, they should be executed before. *)
  mutable rule_time_dependencies :  build_file IntMap.t;
  mutable rule_temporaries : build_file list;
  mutable rule_targets :  build_file IntMap.t;
  mutable rule_state : rule_state;
  mutable rule_missing_sources : int;

  rule_context : build_context;
}

and build_loc = {
    loc_file : string;
    loc_line : int;
    loc_package : build_package;
  }

and dependency_loader =  string -> (string * string list list) list

and  build_action =
    Execute of build_command
  | LoadDeps of dependency_loader *  build_file *  build_rule
  | Copy of command_argument * command_argument
  | Move of only_if_changed * command_argument * command_argument
  | MoveIfExists of command_argument * command_argument
      * command_argument option (* create an ocp-build link *)
  | DynamicAction of string * (build_action list Lazy.t)
  | NeedTempDir
  | Function of string  (* name, for debug *)
      * (Buffer.t -> unit)  (* a printer for the command to compute digests *)
      * (unit -> unit)  (* the function itself *)

and build_command = {
  cmd_command : string list;
  mutable cmd_args : command_argument list;
  mutable cmd_stdout_pipe : string option;
  mutable cmd_stdin_pipe : string option;
  mutable cmd_stderr_pipe : string option;
  mutable cmd_move_to_dir : string option;
}

and command_argument =
    S of string (* string *)
  | T of string  (* temporary file in rule temporary directory *)
  | F of FileGen.t (* FileGen.t *)
  | BF of build_file (* build_file type *)
  | BD of build_directory (* build_file type *)

(* TODO: we should support the fact that directories could also be created by build rules ! *)
and  build_file = {
  file_id : int;
  mutable file_kind : file_kind; (* mutable because we sometimes discover that
                                    a file is virtual afterwards. *)
  file_dir :  build_directory;
  file_file : FileGen.t;
  file_basename : string;
  mutable file_exists : bool;
  mutable file_mtime : BuildMtime.t;
  mutable file_target_of :  build_rule list;
  mutable file_source_for :  build_rule list;
  file_package : build_package;
}

and build_dir_key =
  | Inode of (int * int64)
  | Dirname of string

and build_directory = {
  dir_key : build_dir_key; (* (st_dev, st_ino) *)
  dir_id : int;
  dir_basename : string;
  mutable dir_file : FileGen.t;
  dir_parent :  build_directory;
  mutable dir_files :  build_file StringMap.t;
  mutable dir_dirs :  build_directory StringMap.t;
  mutable dir_fullname : string;
}

and build_package = {
  package_context : build_context;
  package_package : string;
  package_uid : int;
  mutable package_files : build_file IntMap.t;
}

and fatal_error =
  | CopyError of string * string * exn
  | ActionError of string * exn
  | ExecutionError of build_rule * build_command * exn

and error =
  | TargetNotGenerated of build_rule * build_file
  | CommandError of build_rule
                    * int (* step *)
                    * string list (* command *)
                    * string (* stdout *)
                    * string (* stderr *)
  | ExternalDeps of build_file
  | IncorrectDependFile of build_file * exn

and build_context = {
  mutable build_should_restart : bool;

  mutable build_rules : (int, build_rule) Hashtbl.t;
  mutable build_files : (int, build_file) Hashtbl.t;
  mutable build_packages : build_package IntMap.t;

  mutable build_stats_to_execute : int;
  mutable build_stats_executed : int;
  mutable build_stats_running_rules : (build_rule * float) IntMap.t; (* rule id, tstart *)
  mutable build_stats_lastpoint : int;

  mutable build_directories : (int * int64, build_directory) Hashtbl.t;
  mutable build_next_dir_id : int;
  mutable build_next_file_id : int;
  mutable build_next_rule_id : int;
  mutable build_next_process_id : int;
  mutable build_next_package_id : int;

  mutable cross_arg : string option;
  mutable stop_on_error_arg : bool;

  mutable build_dir_basename : string;
  mutable build_dir_filename : string;
  mutable build_dir : FileGen.t;

  mutable build_log : out_channel;

  mutable build_cache_input : Digest.t DigestMap.t;
  mutable build_cache_entries : (Digest.t * Digest.t) IntMap.t;
  mutable build_cache_filename : string;
  mutable build_cache_log : out_channel;

  mutable queue_inactive : build_rule list;
  mutable queue_ready : build_rule IntMap.t;
  mutable queue_waiting : build_rule IntMap.t;
  mutable queue_not_waiting : build_rule IntMap.t;
  mutable temp_files : ( build_rule * build_rule list ref ) IntMap.t;
  mutable unmanaged_dependencies : string list;
  (* TODO: What's the difference between those ? *)
  mutable fatal_errors : fatal_error list;
  mutable errors : error list;

  mutable stats_command_executed : int;
  mutable stats_files_generated : int;
  mutable stats_total_time : float;
  mutable build_create_dirs : build_file list;
  }

type build_process = {
  mutable proc_step : int;
  proc_rule : build_rule;
  mutable proc_commands : build_action list;
  mutable proc_last : build_command option;
}
