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

type package = {
    pk_name : string;
    pk_dirname : string;
    pk_loc : BuildValue.TYPES.location;
    pk_requires : require list;
    pk_rules : rule list;
    pk_env : BuildValue.TYPES.env;
  }

 and require = {
     req_name : string;
     req_env : BuildValue.TYPES.env;
   }

 and rule = {
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
