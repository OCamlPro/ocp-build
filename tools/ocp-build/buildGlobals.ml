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
open BuildTypes
open BuildOCPTypes

(* open BuildEngineContext *)
open BuildEngineRules
open BuildEngineTypes

open BuildValue.TYPES

let verbose = OcpDebug.verbose_function ["B"; "BuildGlobals"]

(* Under Windows, we cannot use dot-prefixed directories *)
let homedir = try Sys.getenv "HOME" with Not_found -> "."

let time_arg = ref false
(*
  let byte_arg = ref false
  let asm_arg = ref false
*)
let clean_arg = ref false

let distclean_arg = ref false
let fake_arg = ref false
let save_config_arg = ref false

let stop_on_error_arg = ref true
let verbosity_arg = ref (None : int option)
let targets_arg = ref ([]: string list)
let distrib_arg = ref false
let conf_arg = ref false
let global_arg = ref false
let no_global_arg = ref false
let autogen_arg = ref false
let list_ocp_files = ref false
let html_report_arg = ref false
let dot_report_arg = ref false

let new_builder_context b = {
  build_context = b;
  build_config_package = BuildEngineContext.new_package b "PROJECT_DESCRIPTION";
  packages_by_name = StringMap.empty;
  all_projects = Hashtbl.create 113;
  config_filename_validated_table = Hashtbl.create 113;
  uniq_rules = Hashtbl.create 113;
}

let config_filename_validated bc lib_loc (filename, digest_o) =
  try
    Hashtbl.find bc.config_filename_validated_table filename
  with Not_found ->
    let b = bc.build_context in
    let basename = Filename.basename filename in
    let dirname = Filename.dirname filename in
    let dir = BuildEngineContext.add_directory b dirname in
    let p = bc.build_config_package in
    let file = BuildEngineContext.add_file p dir basename in
    let file_checked =
      BuildEngineContext.add_virtual_file p dir (basename ^ " checked") in
    let file_validated =
      BuildEngineContext.add_virtual_file p dir (basename ^ " validated") in
    let r_checker = new_rule b lib_loc file_checked [] in
    let r_validator = new_rule b lib_loc file_validated [] in
    add_rule_source r_checker file;
    add_rule_source r_validator file_checked;

    Hashtbl.add bc.config_filename_validated_table filename file_validated;
    let function_name = Printf.sprintf "check %S/%s" filename
        (match digest_o with
           None -> "" | Some digest -> OcpDigest.to_hex digest) in
    add_rule_command r_checker (BuildEngineTypes.Function (function_name, (fun b -> Buffer.add_string b function_name),
        (function () ->
          let digest2_o = try
            let content = FileString.read_file filename in
            Some (Digest.string content)
          with _ -> None
          in
          if digest2_o <> digest_o then begin
            r_validator.rule_missing_sources <- r_validator.rule_missing_sources + 1;
            b.build_should_restart <- true;
            Printf.eprintf "NEED REBOOT\n%!";
          end else begin
(*            Printf.eprintf "%s checked and validated\n%!" filename *)
          end
        )));

    file_validated

let new_library bc pk package_dirname src_dir dst_dir mut_dir =
  let b = bc.build_context in
  (*  let envs = [ pk.package_options ] in *)

  (*  let lib_name = pk.package_name in *)
  let lib_package = BuildEngineContext.new_package b pk.package_name in
  let lib_loc = {
      loc_file = pk.package_filename;
      loc_line = pk.package_loc.BuildValue.TYPES.loc_begin.Lexing.pos_lnum;
      loc_package = lib_package;
    }
  in

  let lib =
    {
      lib_builder_context = bc;
      lib_context = b;
      lib_package;
      lib_id = pk.package_id;
      lib_name = pk.package_name;
      lib_loc;
      lib_source_kind = pk.package_source_kind;
      lib_dirname = FileGen.of_string package_dirname;
      lib_type = pk.package_type ;
      lib_tag = "";
      lib_filename = pk.package_filename;
      lib_node = pk.package_node;
      lib_added = pk.package_disabled = None;

      lib_src_dir = src_dir;
      lib_dst_dir = dst_dir;
      lib_mut_dir = mut_dir;
      lib_bundles = [];
    } in

  Hashtbl.add bc.all_projects lib.lib_id lib;
  bc.packages_by_name <- StringMap.add lib.lib_name lib bc.packages_by_name;
  if verbose 5 then begin
    Printf.eprintf "BuildGlobals.new_library %S\n" lib.lib_name;
  (*    Printf.eprintf "  lib_install = %b\n%!" lib.lib_install; *)
  end;
  lib

let absolute_filename dirname =
  if Filename.is_relative dirname then
    Filename.concat (BuildMisc.getcwd ()) dirname
  else dirname


(* This is the arg_list used when the subcommand is
   called. Plugins can add arguments here *)
let arg_list = ref ([] : (string * Arg.spec * string) list)

let ocpbuild_version =
  BuildValue.new_version_option "ocpbuild_version" BuildVersion.version
