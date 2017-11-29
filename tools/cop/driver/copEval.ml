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

exception BadRule of BuildValue.TYPES.location * string * BuildValue.TYPES.value
exception BadRequire of BuildValue.TYPES.location * BuildValue.TYPES.value


module Init = struct

  type context = CopTypes.context

  let parse_error () =
    exit 2

  let new_file ctx filename digest =
    try
      let digest2 = StringMap.find filename ctx.c_files in
      if digest <> digest2 then begin
          Printf.eprintf "File %S modified during built. Exiting.\n%!" filename;
          exit 2
        end
    with Not_found ->
      ctx.c_files <-
        StringMap.add filename digest ctx.c_files

  end

module Eval : sig

  val add_primitive :
    string ->
    string list ->
    (BuildValue.TYPES.location ->
     CopTypes.context ->
     BuildValue.TYPES.config ->
     BuildValue.TYPES.value list -> BuildValue.TYPES.value) ->
    unit

  val read_ocamlconf :
    string ->
    CopTypes.context ->
    BuildValue.TYPES.config ->
    BuildValue.TYPES.config

end = BuildOCP2Interp.Eval(Init)

let add_prim = Eval.add_primitive
let eval_file state config file =
  Eval.read_ocamlconf file state config

let init_state () =
  {
    c_files = StringMap.empty;
    c_switches = StringMap.empty;
    c_packages = StringMap.empty;
    c_switch = None;
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

let load_projects c sw config project_files =
  c.c_switch <- Some sw;
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
                eval_file c config file
              with BuildMisc.ParseError ->
                incr nerrors;
                config
            in
            iter ( (dirname, file, config) :: parents ) next_files
            else
              iter next_parents files
  in
  iter [ "", "<root>", config ] project_files;
  c.c_switch <- None;
  !nerrors



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

let parse_rule r_package rule =
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
                       r_package;
       r_targets = [];
       r_sources = [];
       r_temps = [];
       r_commands = [];
     }
  | _ -> raise Not_found

let parse_rule pk loc rule =
  try
    parse_rule pk rule
  with _ ->
    raise (BadRule (loc,pk.pk_name, rule))

          (*
let parse_require loc = function
  | VString (req_name,_) ->
     { req_name; req_env = empty_env }
  | VTuple[ VString (req_name,_);
            ( VObject req_env
              | VList [VObject req_env]
          )] ->
     { req_name; req_env }
  | r ->
     raise (BadRequire (loc,r))
           *)

let parse_info _pk _pk_loc _pk_info = assert false

let rec insert_package pk old_pk =
  if pk.pk_priority > old_pk.pk_priority then begin
      pk.pk_sibling <- Some old_pk;
      pk
    end else
    match old_pk.pk_sibling with
    | None ->
       old_pk.pk_sibling <- Some pk;
       old_pk
    | Some old_sibling ->
       old_pk.pk_sibling <- Some (insert_package pk old_sibling);
       old_pk

let add_project c pk_loc pk_name pk_config
                pk_info pk_description =
  let pk_switch =
    match c.c_switch with
    | None -> Printf.kprintf failwith "package %S declared outside switch"
                             pk_name
    | Some sw -> sw
  in
  let pk_name = pk_switch.sw_name ^ ":" ^ pk_name in
  let pk_info = match pk_info with
    | VObject pk_info -> pk_info
    | VList _ ->
       BuildValue.set BuildValue.empty_env "requires" pk_info
    | _ ->
       Printf.kprintf failwith "package %S bad info type" pk_name
  in

  let pk_dirname =
    try
      let dirname = BuildValue.get_string
                      [pk_info; pk_config.config_env] "dirname" in
      if Filename.is_relative dirname then
        Filename.concat pk_config.config_dirname dirname
      else
        dirname
    with Var_not_found _ ->
      pk_config.config_dirname
  in
  (*
  let pk_rules = BuildValue.get_with_default [pk_env] "rules" (VList[]) in
    let pk_rules = parse_list (parse_rule pk_loc) pk_rules in
   *)
  let pk_node = OcpToposort.new_node () in
  let pk = {
      pk_switch;

      pk_name;
      pk_dirname;
      pk_loc;
      pk_node;

      pk_requires = [];
      pk_priority = 0;
      pk_enabled = true;
      pk_sibling = None;

      pk_info;
      pk_description;

      pk_envs = [];
      pk_rules = [];
    } in
  parse_info pk pk_loc pk_info;
  pk_switch.sw_packages <- pk :: pk_switch.sw_packages;
  c.c_packages <-
    StringMap.add pk_name
                  (try
                     let old_pk = StringMap.find pk_name c.c_packages
                     in
                     insert_package pk old_pk
                   with Not_found -> pk)
                  c.c_packages
