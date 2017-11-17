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
open CopTypes

type state = {
    mutable config_files : string StringMap.t;
    mutable package_descriptions : package list;
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



let empty_env = BuildValue.empty_env

let empty_command_options = {
    c_stdin = None;
    c_stdout = None;
    c_stderr = None;
    c_chdir = None;
  }

let empty_atom_options = {
    a_package = None;
    a_virtual = None;
    a_subst = None;
  }

let parse_subst = function
  | VObject env -> Some env
  | VBool true -> Some empty_env
  | VBool false -> None
  | _ -> raise Not_found

let parse_bool = function
  | VBool bool -> bool
  | _ -> raise Not_found

let parse_string = function
  | VString (s,_) -> s
  | _ -> raise Not_found

let parse_atom_options env =
  BuildValue.fold (fun field value c ->
      match field with
      | "package" ->
         { c with a_package = Some (parse_string value) }
      | "virtual" ->
         { c with a_virtual = Some (parse_bool value) }
      | "subst" ->
         { c with a_subst = parse_subst value }
      | _ ->
       Printf.kprintf failwith "Invalid field %S in atom options" field
    ) env empty_atom_options

let parse_atom = function
  | VString (file, _) -> (file, empty_atom_options)
  | VTuple [VString (file, _); VObject env] ->
     (file, parse_atom_options env)
  | _ -> raise Not_found

let parse_chdir = function
  | VBool true -> Some RunInPackageSrc
  | VBool false -> None
  | value -> Some (RunIn (parse_atom value))

let parse_command_options env =
  BuildValue.fold (fun field value c ->
      match field with
      | "stdin" ->
         { c with c_stdin = Some (parse_atom value) }
      | "stdout" ->
         { c with c_stdout = Some (parse_atom value) }
      | "stderr" ->
         { c with c_stderr = Some (parse_atom value) }
      | "chdir" ->
         { c with c_chdir = parse_chdir value }
      | _ ->
       Printf.kprintf failwith "Invalid field %S in command options" field
    ) env empty_command_options

let parse_command = function
  | VTuple [ VString("shell:",_); VList shell ] ->
     Shell (List.map parse_atom shell, empty_command_options)
  | VTuple [ VString("shell:",_); VList shell; VObject env ] ->
     Shell (List.map parse_atom shell, parse_command_options env)
  | _ -> raise Not_found

let parse_list parse_fun list =
  match list with
      VList list -> List.map parse_fun list
    | ele -> [parse_fun ele]

let parse_rule rule =
  match rule with
  | VObject env ->

     BuildValue.fold (fun field value r ->
         match field with
         | "targets" ->
            let r_targets = parse_list parse_atom value in
            { r with r_targets }
         | "sources" ->
            let r_sources = parse_list parse_atom value in
            { r with r_sources }
         | "temps" ->
            let r_temps = parse_list parse_atom value in
            { r with r_temps }
         | "commands" ->
            let r_commands = parse_list parse_command value in
            { r with r_commands}
         | _ ->
            Printf.kprintf failwith "Invalid field %S in rule" field
       ) env
     {
       r_targets = [];
       r_sources = [];
       r_temps = [];
       r_commands = [];
     }
  | _ -> raise Not_found

exception BadRule of BuildValue.TYPES.value

let parse_rule rule =
  try
    parse_rule rule
  with _ ->
    raise (BadRule rule)

let parse_require = function
  | VString (req_name,_) ->
     { req_name; req_env = empty_env }
  | VTuple[ VString (req_name,_); VObject req_env] ->
     { req_name; req_env }
  | _ -> raise Not_found

let add_project state pk_loc pk_name pk_config pk_requires pk_env =
  let pk_dirname =
    try
      let dirname = BuildValue.get_string
                      [pk_env; pk_config.config_env] "dirname" in
      if Filename.is_relative dirname then
        Filename.concat pk_config.config_dirname dirname
      else
        dirname
    with Var_not_found _ ->
      pk_config.config_dirname
  in
  let pk_requires =
    List.map parse_require pk_requires
  in
  let pk_rules = BuildValue.get_with_default [pk_env] "rules" (VList[]) in
  let pk_rules = parse_list parse_rule pk_rules in
  let p = {
      pk_name;
      pk_dirname;
      pk_loc;
      pk_requires;
      pk_env;
      pk_rules;
    } in
  state.package_descriptions <- p :: state.package_descriptions
