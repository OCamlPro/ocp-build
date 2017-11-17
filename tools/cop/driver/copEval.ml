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
open BuildValue.TYPES

       (*
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
     req_options : BuildValue.TYPES.env;
   }

 and rule = {
     r_targets : atom list;
     r_sources : atom list;
     r_temps : atom list;
     r_commands : command list;
   }

 and command =
   | Shell of string list * command_options
   | Builtin of (unit -> unit)

 and command_options = {
     c_stdin : atom option;
     c_stdout : atom option;
     c_stderr : atom option;
     c_chdir : chdir;
   }

 and chdir =
   | RunInPackageSrc
   | RunIn of atom

 and atom = string * atom_options

 and atom_options = {
     a_package : string option;
     a_virtual : bool option;
   }
        *)

type package_description = {
    pk_name : string;
    pk_requires : BuildValue.TYPES.value list;
    pk_env : BuildValue.TYPES.env;
    pk_config : BuildValue.TYPES.config;
    pk_loc : BuildValue.TYPES.location;
  }

type state = {
    mutable config_files : string StringMap.t;
    mutable package_descriptions : package_description list;
  }


module Init = struct

  type context = state

  let parse_error () =
    exit 2

  let new_file ctx filename digest =
    try
      let digest2 = StringMap.find filename ctx.config_files in
      if digest <> digest2 then begin
          Printf.eprintf "File %S modified during built. Exiting.\n%!" filename;
          exit 2
        end
    with Not_found ->
      ctx.config_files <-
        StringMap.add filename digest ctx.config_files

  end

module Eval : sig

  val add_primitive :
    string ->
    string list ->
    (BuildValue.TYPES.location ->
     state ->
     BuildValue.TYPES.config ->
     BuildValue.TYPES.value list -> BuildValue.TYPES.value) ->
    unit

  val read_ocamlconf :
    string ->
    state ->
    BuildValue.TYPES.config ->
    BuildValue.TYPES.config

end = BuildOCP2Interp.Eval(Init)

let add_prim = Eval.add_primitive
let eval_file state config file =
  Eval.read_ocamlconf file state config

let init_state () =
  {
    config_files = StringMap.empty;
    package_descriptions = [];
  }

let init_workspace state module_files =

  let config = BuildValue.empty_config () in

  List.iter (fun file ->

      let (_ : BuildValue.TYPES.config) = eval_file state config file in

      let basename = Filename.basename file in
      let modname =
        String.capitalize (Filename.chop_suffix basename ".cop")
      in
      if not (StringMap.mem modname config.config_state.cfs_modules)
         &&
           (let prefixed_modname =
              let dirname = Filename.basename (Filename.dirname file) in
              Printf.sprintf "%s:%s" dirname modname
            in
            not (StringMap.mem prefixed_modname
                               config.config_state.cfs_modules))
      then begin
          Printf.eprintf "Warning: file %S did not define module %S\n%!"
                         file modname;
        end
    ) module_files;

  state, config

let load_projects state config project_files =
  let state = { state with package_descriptions = [] } in
  let nerrors = ref 0 in
  let rec iter parents files =
    match files with
      [] -> ()
    | file :: next_files ->
       match parents with
         [] -> assert false
       | (parent, filename, config) :: next_parents ->
          if OcpString.starts_with file ~prefix:parent then
            let dirname = Filename.dirname file in
            if !CopGlobals.verbose >= 5 then
              Printf.eprintf "Reading %s with context from %s\n%!"
                             file filename;
            let config =
              try
                eval_file state config file
              with BuildMisc.ParseError ->
                incr nerrors;
                config
            in
            iter ( (dirname, file, config) :: parents ) next_files
            else
              iter next_parents files
  in
  iter [ "", "<root>", config ] project_files;
  !nerrors, state.package_descriptions

let add_project state pk_loc pk_name pk_config pk_requires pk_env =
  let p = {
      pk_name;
      pk_requires;
      pk_env;
      pk_config;
      pk_loc;
    } in
  state.package_descriptions <- p :: state.package_descriptions
