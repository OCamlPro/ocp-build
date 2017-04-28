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
open BuildOCamlTypes
open BuildOptions
open BuildOCamlVariables
open BuildValue.TYPES

let list_byte_targets_arg = ref false
let list_asm_targets_arg = ref false

let ocaml_packages = Hashtbl.create 111

let reset () =
  Hashtbl.clear ocaml_packages

let create_package lib opk =
  let envs = opk.opk_options in

  let b = lib.lib_context in
  let p = lib.lib_package in
  let bc = lib.lib_builder_context in
  let pk = opk.opk_package in

  let lib_archive = BuildValue.get_string_with_default envs "archive" lib.lib_name in
  let lib_stubarchive = BuildValue.get_string_with_default envs "stubarchive" ("ml" ^ lib_archive) in

  (*
  let lib_sources = BuildValue.get_local_prop_list_with_default envs "files" [] in
  let lib_tests = BuildValue.get_local_prop_list_with_default envs "tests" [] in
  *)

  let lib_requires = List.map (fun dep ->
      let pk2 = dep.dep_project.opk_package in
      let lib2 =
        try
          Hashtbl.find ocaml_packages pk2.package_id
        with Not_found ->
          Printf.eprintf "Unknown dependency %d (%s) of package %S\n%!"
            pk2.package_id
            pk2.package_name
            lib.lib_name;
          BuildMisc.clean_exit 2
      in
      { dep with dep_project = lib2 }
    ) opk.opk_requires
in

  let lib_autolink = match lib.lib_type with
    | TestPackage
    | ObjectsPackage
    | RulesPackage
      -> BuildValue.get_bool_with_default envs "autolink" false
    | ProgramPackage
    | LibraryPackage
    | SyntaxPackage ->
      BuildValue.get_bool_with_default envs "autolink" true
  in


  let lib_ready =
    if opk.opk_installed then [] else
      let file_ready =
        BuildEngineContext.add_virtual_file p lib.lib_dst_dir
          (lib.lib_name ^ " validated") in
      let r = BuildEngineRules.new_rule b lib.lib_loc file_ready [] in
      List.iter (fun filename ->
        BuildEngineRules.add_rule_source r
          (BuildGlobals.config_filename_validated bc lib.lib_loc filename)
      ) pk.package_filenames;
      [file_ready]
  in
  let lib_meta = BuildValue.get_bool_with_default envs "meta" false in


  let lib = {
    lib = lib;
    lib_opk = opk;

    lib_autolink;

    lib_byte_targets = [];
    lib_asm_targets = [];
    lib_intf_targets = [];
    lib_stub_targets = [];

    lib_modules = [];
    lib_internal_modules = StringsMap.empty;
    (* lib_dep_deps = IntMap.empty; *)
    lib_includes = None;
    lib_linkdeps = [];
    lib_sources = BuildValue.get_local_prop_list_with_default envs "files" [];
    lib_tests = BuildValue.get_local_prop_list_with_default envs "tests" [];

    lib_doc_targets = ref [];
    lib_test_targets = ref [];
    lib_build_targets = ref [];


    lib_archive;
    lib_stubarchive;

    lib_ready;
    lib_meta ;

    lib_requires;
  }
  in
  Hashtbl.add ocaml_packages lib.lib.lib_id lib;
  if BuildGlobals.verbose 5 then begin
    Printf.eprintf "BuildOCamlGlobals.create_package %S\n" lib.lib.lib_name;
    List.iter (fun (s, _) ->
      Printf.eprintf "  MOD %S\n%!" s;
    ) lib.lib_sources;
  end;
  lib

let get_by_id lib =
  try
    Some (Hashtbl.find ocaml_packages lib.lib_id)
  with Not_found -> None

let get_by_name bc name =
  try
    let lib =
      StringMap.find name bc.packages_by_name
    in
    get_by_id lib
  with Not_found -> None

let make_build_targets lib cin =
  match get_by_id lib with
  | None -> []
  | Some lib ->
    (if cin.cin_bytecode then
      List.map fst lib.lib_byte_targets
    else []) @
      (if cin.cin_native then
          List.map fst lib.lib_asm_targets
       else []) @
      (List.fold_left (fun list (file, kind) ->
        match kind with
        | CMI -> file :: list
        | CMX when cin.cin_native -> file :: list
        | _ -> list
       ) [] lib.lib_intf_targets) @
      (List.map fst lib.lib_stub_targets) @
      !(lib.lib_build_targets)

let make_doc_targets lib _cin =
   match get_by_id lib with
  | None -> []
  | Some lib -> !(lib.lib_doc_targets)

let make_test_targets lib _cin =
   match get_by_id lib with
  | None -> []
  | Some lib -> !(lib.lib_test_targets)
