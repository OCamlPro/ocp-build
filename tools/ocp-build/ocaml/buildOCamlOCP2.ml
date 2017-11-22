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
open BuildOCPTypes
open BuildOCamlTypes
open BuildValue.TYPES

(* let verbose = OcpDebug.verbose_function [ "B"; "BP"; "BuildOCamlPackage"] *)

let add_primitive name prim_help prim =
  let prim_name = "OCaml_" ^ name in
  BuildOCamlVariables.ocamlmod_add name (VFun (VPrim prim_name));
  BuildOCP.add_primitive prim_name prim_help prim


let is_enabled options =
  BuildValue.get_bool_with_default options "enabled" true

let add_ocaml_package loc state config name kind =
  (*  Printf.eprintf "BuildOCamlPackage.add_ocaml_package %S\n%!" name; *)
  let (pk : unit BuildOCPTypes.package) =
    BuildOCP.define_package loc state config ~name ~kind
  in
  let env = config.config_env in
  let options = [env] in
  let opk_version =
    BuildValue.get_string_with_default options "version"  "0.1-alpha" in
  let opk_installed = BuildValue.is_already_installed options in
  let opk_install =
    not opk_installed &&
      (BuildValue.get_bool_with_default options "install"
         (match pk.package_type with
           TestPackage -> false
         | ProgramPackage
         | LibraryPackage
         | ObjectsPackage
         | RulesPackage
         | SyntaxPackage -> true))
  in
  let opk_has_byte = BuildOCamlVariables.byte_option.get options in
  let opk_has_asm = BuildOCamlVariables.asm_option.get options in

  let opk = {
    opk_name = name;
    opk_package = pk;
    opk_options = options;
    opk_dirname = pk.package_dirname;
    opk_kind = pk.package_type;

    opk_install;
    opk_installed;
    opk_has_byte;
    opk_has_asm;

    opk_tolink = false;
    opk_program = false;
    opk_library = false;
    opk_build = true;
    opk_test = false;
    opk_syntax = false;

    opk_version;
    opk_id  = 0;
    opk_requires = [];

    opk_requires_map = StringMap.empty;
    opk_usedby_map = StringMap.empty;
  } in

  begin
    match pk.package_type with
    | ProgramPackage ->
      opk.opk_program <- true;
    | LibraryPackage ->
      opk.opk_tolink <- true;
      opk.opk_library <- true;
    | ObjectsPackage ->
      opk.opk_tolink <- true;
    | RulesPackage ->
      opk.opk_build <- opk.opk_tolink
    | TestPackage ->
      opk.opk_build <- false;
      opk.opk_test <- true;
    | SyntaxPackage ->
      opk.opk_syntax <- true
  end;
  opk.opk_tolink <-
    BuildValue.get_bool_with_default options "tolink" opk.opk_tolink;
  opk.opk_program <-
    BuildValue.get_bool_with_default options "program" opk.opk_program;
  opk.opk_library <-
    BuildValue.get_bool_with_default options "library" opk.opk_library;

  pk.package_plugin <- OCamlPackage opk;
  if not ( is_enabled options ) then
    pk.package_disabled <- Some "enabled=false in config";
  (*
  let s = BuildOCPPrinter.string_of_package (fun _ _ _ -> ()) pk in
  Printf.eprintf "New OCaml Description:\n";
  Printf.eprintf "%s\n%!" s;
  *)
  opk

let add_ocaml_package_unit loc state config name kind =
  let (_ : ocaml_description) =
    add_ocaml_package  loc state config name kind in
  ()

let add_ocaml_package_pk loc state config name kind =
  let opk = add_ocaml_package  loc state config name kind in
  opk.opk_package


let _ =

  add_primitive "new_package"
    [ "Add a new OCaml package" ]
    (fun loc state config args ->
      match args with
      | [ VString (name,_); VString (kind,_); VObject config_env ] ->
        add_ocaml_package_unit loc state { config  with config_env }
          name (BuildOCP.package_type_of_string kind);
        BuildValue.unit
      |  _ ->
        BuildOCP2Prims.raise_bad_arity loc
          "OCaml.new_package(name,kind,ocaml)" 3 args
    );
  add_primitive "library"
    [ "Add a new OCaml library" ]
    (fun loc state config args ->
      match args with
      | [ VString (name,_); VObject config_env ] ->
        add_ocaml_package_unit loc state { config  with config_env }
          name BuildOCPTypes.LibraryPackage;
        BuildValue.unit
      |  _ ->
        BuildOCP2Prims.raise_bad_arity loc
          "OCaml.library(name,ocaml)" 2 args
    );
  add_primitive "program"
    [ "Add a new OCaml program" ]
    (fun loc state config args ->
      match args with
      | [ VString (name,_); VObject config_env ] ->
        add_ocaml_package_unit loc state { config  with config_env }
          name BuildOCPTypes.ProgramPackage;
        BuildValue.unit
      |  _ ->
        BuildOCP2Prims.raise_bad_arity loc
          "OCaml.library(name,ocaml)" 2 args
    );
  add_primitive "rules"
    [ "Add a new set of OCaml rules" ]
    (fun loc state config args ->
      match args with
      | [ VString (name,_); VObject config_env ] ->
        add_ocaml_package_unit loc state { config  with config_env }
          name BuildOCPTypes.RulesPackage;
        BuildValue.unit
      |  _ ->
        BuildOCP2Prims.raise_bad_arity loc
          "OCaml.rules(name,ocaml)" 2 args
    );

  add_primitive "objects"
    [ "Add a new set of OCaml objects" ]
    (fun loc state config args ->
      match args with
      | [ VString (name,_); VObject config_env ] ->
        add_ocaml_package_unit loc state { config  with config_env }
          name BuildOCPTypes.ObjectsPackage;
        BuildValue.unit
      |  _ ->
        BuildOCP2Prims.raise_bad_arity loc
          "OCaml.objects(name,ocaml)" 2 args
    );

  add_primitive "system"
    [ "Call a shell command" ]
    (fun loc _state _config args ->
      match args with
      | [ VList cmd ] ->
          VTuple [ VString ("", StringRaw);
                   BuildValue.new_object [ "value", VList cmd ] ]
      | [ VList cmd; VObject { env } ] ->
        let env = StringMap.add "value" (VList cmd) env in
        VTuple [ VString ("", StringRaw);
                 VObject { env } ]
      |  _ ->
        BuildOCP2Prims.raise_bad_arity loc
          "OCaml.system(cmd [,options])" 1 args
    );

  add_primitive "pack" [
    "pack(string[,pack_env], list-of-strings)"
  ] (fun loc _ctx _config args ->
    let packmodname, pack_env, files =
      match args with
      | [VString (packmodname,_); files] ->
        (packmodname, BuildValue.empty_env, files)
      | [VString (packmodname,_); VObject pack_env; files] ->
        (packmodname, pack_env, files)
      | _ ->
        BuildOCP2Prims.raise_bad_arity loc "pack(name, files)" 2 args
    in
    let files = BuildValue.prop_list files in
    let modnames = ref [] in

    let files = List.map (fun (file, file_env) ->
      file,
      BuildValue.set_strings file_env "packed"
        (packmodname ::
           (try
              BuildValue.get_strings [ file_env ] "packed"
            with Var_not_found _ ->
              modnames := Filename.basename file :: !modnames;
              []))
    ) files in

    let pack_env = BuildValue.set_strings pack_env "pack" (List.rev !modnames) in

    BuildValue.value (files @
                        [ packmodname ^ ".ml", pack_env ])
  );
  ()

(* This [getconf] primitive should probably be moved to BuildOCP,
as it is not specific to OCaml, isn't it ? *)
let () =
  BuildOCP.add_primitive "getconf"
    [ "getconf(name) returns the configuration associated with name" ]
    (fun loc _state config args ->
      match args with
      | [VString (name,_)] ->
        begin
          try
            begin match BuildValue.config_get config "config" with
            | VObject env ->
              BuildValue.get [env] name
            | _ -> BuildValue.unit
            end
          with Var_not_found _ -> BuildValue.unit
        end
      | _ ->
        BuildOCP2Prims.raise_bad_arity loc
          "getconf(name)" 1 args
    )

let pk_opk pk =
  match pk.package_plugin with
  | OCamlPackage opk -> opk
  | _ -> assert false

let predefined_packages = ref (VObject BuildValue.empty_env)

let () =
  add_primitive "findlib"
    [ "Returns the map of pre-existing packages" ]
    (fun _loc _state _config _args ->
      !predefined_packages
        )

let init_env env_pj =

  BuildOCamlVariables.packages_option.set
    (VList (Array.to_list (Array.map (fun pk ->
      let opk = pk_opk pk in
      let dirname = BuildGlobals.absolute_filename pk.package_dirname in
      List.iter (fun suffix ->
        BuildSubst.add_to_global_subst (pk.package_name ^ suffix) dirname)
        [ "_SRC_DIR"; "_DST_DIR"; "_FULL_SRC_DIR"; "_FULL_DST_DIR" ];
      VTuple [
        VString (pk.package_name, StringRaw);
        VString (opk.opk_version, StringVersion);
        VString (dirname, StringRaw);
        VObject (List.hd opk.opk_options)]
                             ) env_pj.project_sorted)));

  let env = ref BuildValue.empty_env in
  Array.iter (fun pk ->
      let opk = pk_opk pk in
      let dirname = BuildGlobals.absolute_filename pk.package_dirname in
      let v =
        List.fold_left (fun env (name, value) ->
            BuildValue.set env name value)
                       (List.hd opk.opk_options)
                       [
                         "name", VString (pk.package_name, StringRaw);
                         "version", VString (opk.opk_version, StringVersion);
                         "dirname", VString (dirname, StringRaw);
                       ]
      in
      env := BuildValue.set !env pk.package_name (VObject v)
    ) env_pj.project_sorted;
  predefined_packages := VObject !env;

  ()



let () =
  BuildOCP.plugin_verifiers :=
    BuildOCamlVerifyPackages.verify_packages :: !BuildOCP.plugin_verifiers;

  (* Obsolete: for .ocp files, we can only define OCaml packages, so
     BuildOCP has a specific way to do it ! *)
  BuildOCP.add_ocaml_package := add_ocaml_package_pk;

  ()

let init () = ()
