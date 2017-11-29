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


(* We should have package descriptions at this level ! *)
type context = {
    mutable c_files : string StringMap.t;
    mutable c_switches : switch StringMap.t;

    (* The current switch when evaluating project descriptions *)
    mutable c_switch : switch option;
    mutable c_packages : package StringMap.t;
  }

and switch = {
    sw_context : context;
    sw_name : string;
    mutable sw_host : switch;
    mutable sw_build : switch;
    mutable sw_packages : package list;
  }

and package = {
    pk_switch : switch;

    pk_name : string;
    pk_dirname : string;
    pk_loc : BuildValue.TYPES.location;
    pk_node : OcpToposort.node;

    pk_requires : require list;
    pk_priority : int;
    pk_enabled : bool;
    (* Maybe another *less* important description of this package: *)
    mutable pk_sibling : package option;


    pk_info : BuildValue.TYPES.env;
    pk_description : BuildValue.TYPES.value;

    (* Computed from DESCRIPTION avec sorting *)
    mutable pk_envs : BuildValue.TYPES.env list;
    mutable pk_rules : rule list;
  }

 and require = {
     req_name : string;
     req_env : BuildValue.TYPES.env;
   }

 and rule = {
     r_package : package;
     r_targets : atom list;
     r_sources : atom list;
     r_temps : atom list;
     r_commands : command list;
   }

 and command =
   | Shell of atom list * command_options
   | Builtin of (unit -> unit)

 and command_options = {
     c_stdin : atom option;
     c_stdout : atom option;
     c_stderr : atom option;
     c_chdir : chdir option;
   }

 and chdir =
   | RunInPackageSrc
   | RunIn of atom

 and atom = string * atom_options

 and atom_options = {
     a_package : string option;
     a_virtual : bool option;
     a_subst : BuildValue.TYPES.env option;
   }
