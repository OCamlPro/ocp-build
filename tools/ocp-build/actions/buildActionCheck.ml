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

let command_name = "check"
let command_help = {|
ocp-build check [CHECK_OPTIONS] [CONFIGURE_OPTIONS]

Read the project description and issue warnings. Do not modify anything in the project.
|}


open OcpCompat

open BuildArgs

open SimpleConfig.Op

open BuildEngineTypes
open BuildTerm (* Terminal functions *)
open BuildOptions (* cin_... *)
open BuildOCamlConfig.TYPES
open BuildOCPTypes
open BuildValue.TYPES
open BuildActions
open BuildTypes

open Ezcmd.Modules

let print_queried_features_arg = ref false


let verbose = OcpDebug.verbose_function ["B"; "BuildActionInit"]

let do_load_project_files cin project_dir state config_state =
  let open BuildOptions.ProjectOptions in

  let force_scan = ref cin.cin_autoscan in

  (* if we didn't find any .ocp files before, we should retry ! *)
  if !!root_files = [] then force_scan := true;

  if ! add_external_projects_arg <> [] then begin
    List.iter (fun dir ->
      if not (List.mem dir !!project_external_dirs_option) then begin
        must_save_project ();
        project_external_dirs_option =:= !!project_external_dirs_option @
            [ dir ];
        force_scan := true;
      end
    ) (List.rev !add_external_projects_arg)
  end;

  if !!project_ocpbuild_version != BuildVersion.version then begin
    must_save_project ();
    project_ocpbuild_version =:= BuildVersion.version;
  end;

  let nerrors =
    if !oasis_arg then
      BuildOasis.load_project state "_oasis"
    else
      begin
      if !force_scan then begin
        save_project := true;
        BuildActions.time_step "Scanning project for .ocp files ...";
        root_files =:= [];
        List.iter (fun dir ->
          let files = BuildOCP.scan_root dir in
          root_files =:= !!root_files @ files
        ) (project_dir ::
            (List.map FileGen.of_string !!project_external_dirs_option));
        BuildActions.time_step "   Done scanning project for .ocp files";
      end;

      if !!root_files = [] then begin
        Printf.eprintf "Error: no known .ocp files\n";
        Printf.eprintf "\tHave you run ocp-build with -scan to find them ?\n%!";
        BuildMisc.clean_exit 2
      end;

      BuildActions.time_step "Loading project .ocp files...";
      let nerrors =
        let config = BuildOCP.empty_config () in
        let config_state = { config_state with
          cfs_modules = config_state.cfs_modules
        } in
        let config = { config with config_state } in
        BuildOCP.load_ocp_files config state !!root_files
      in
      BuildActions.time_step "   Done loading project .ocp files";

      if !print_queried_features_arg then begin
          let features = BuildOCP2Prims.queried_features() in
          Printf.printf "Queried features (--with-help):\n";
          StringMap.iter (fun s enabled ->
              Printf.printf "  * %S (default: %s)\n" s
                            (if enabled then "enabled" else "disabled")
            ) features;
          Printf.printf "Done.%!";
          BuildMisc.clean_exit 0;
        end;

      nerrors
      end
  in
  if nerrors > 0 then BuildMisc.clean_exit 2


let do_print_project_info pj =

  let string_of_package pj =
    Printf.sprintf "   %s (%s,%s)\n     in %s\n"
      pj.package_name
      (BuildOCP.string_of_package_type pj.package_type)
      pj.package_source_kind
      pj.package_dirname
  in
  (*
  let print_package pj =
    Printf.eprintf "%s\tdeps:" (string_of_package pj);
    List.iter (fun s ->
      Printf.eprintf " %s" s;
    ) (BuildValue.get_strings_with_default [pj.package_options] "requires" []);
    Printf.eprintf "\n%!";
  in
  *)
  if verbose 5 || !list_projects_arg then begin

    let print_package_array array =
      let list = ref [] in
      Array.iter (fun pj ->
        list := string_of_package pj :: !list) array;
      List.iter (fun s ->
        Printf.eprintf "%s%!" s)
        (List.sort compare !list)
    in

    Printf.eprintf "Validated packages:\n";
    print_package_array pj.project_sorted;

    Printf.eprintf "Disabled packages:\n";
    print_package_array pj.project_disabled;

  end;

  (*
  begin
    let incomplete_packages = Hashtbl.create  13 in
    if pj.project_incomplete <> [||] then begin
      Printf.eprintf "Warning: %d incomplete packages (will not be built):\n"
        (Array.length pj.project_incomplete);
      let meta_need = ref 0 in
      Array.iter (fun pk ->
        Hashtbl.add incomplete_packages pk.package_name pk;
        if !meta_verbose_arg ||
           pk.package_source_kind <> "meta" then (* TODO ? *)
          print_package pk
        else
          incr meta_need
      )
        pj.project_incomplete;
      if !meta_need > 0 then
        Printf.eprintf
          "  Hidden: %d incomplete packages in META files (use -print-incomplete-meta).\n%!" !meta_need
    end;

    if pj.project_missing <> [] then
      let absent_packages = ref [] in
      let other_packages = ref [] in
      List.iter (fun (name, list) ->
        let non_meta_need = ref false in
        if !meta_verbose_arg then
          non_meta_need := true
        else
          List.iter (fun pk ->
            if pk.package_source_kind <> "meta" then non_meta_need := true
          ) list;
        if !non_meta_need then begin
          let packages =
            if Hashtbl.mem incomplete_packages name then
              other_packages else absent_packages in
          packages := (name, list) :: !packages
        end;
      ) pj.project_missing;
      if !absent_packages <> [] then begin
        Printf.eprintf "Warning: %d needed packages are missing !\n%!"
          (List.length !absent_packages);
        List.iter (fun (name, list) ->
          Printf.eprintf "   ABSENT package %S missed by %d packages\n"
            name (List.length list);
          List.iter print_package list;
        ) !absent_packages
      end;
      List.iter (fun (name, list) ->
        Printf.eprintf "   Incomplete package %S missed by %d packages\n"
          name
          (List.length list);
        List.iter print_package list;
      ) !other_packages

  end;
  *)
  ()

(* TODO: we should return two lists:
  * the list of targets that can be built
  * the list of targets that cannot be built
 *)

let do_print_fancy_project_info _pj =
  let cantbuild = [] in
  (*
  let missing =
    List.filter
      (fun (_name, pkgs) ->
        List.exists (fun pk -> pk.package_source_kind <> "meta") pkgs)
      pj.project_missing
  in
  (* don't complain if there is no problem with the selected targets *)
    let missing_roots =
    (* remove all missing pkgs that depend on another to get the missing roots *)
      List.filter
        (fun (name, _pkgs) ->
          not
            (List.exists
               (fun (_,pks) ->
                 List.exists (fun pk -> name = pk.package_name) pks)
               missing))
        missing
    in
    let cantbuild =
      if missing = [] then cantbuild
      else if missing_roots = [] then begin (* no roots ! *)
        let rec find_cycle acc = function
          | [] -> None
          | name :: _ when List.mem name acc -> Some acc
          | name :: r ->
            let provides =
              List.map (fun pk -> pk.package_name)
                (try List.assoc name missing with Not_found -> [])
            in
            match find_cycle (name::acc) provides with
            | Some _ as r -> r
            | None -> find_cycle acc r
        in
        let cycle = List.map fst missing in
        let cycle =
          match find_cycle [] cycle with
          | Some l -> l
          | None -> assert false
        in
      (*TODO: these are only errors if the corresponding packages have
        been specified as targets. *)
        Printf.eprintf
          "%sERROR%s: circular dependency between:\n"
          term.esc_red_text term.esc_end;
        List.iter
          (fun (n1,n2) -> Printf.eprintf "  - %s%s%s depends on %s\n"
            term.esc_bold n1 term.esc_end n2)
          (List.combine cycle (List.tl cycle @ [List.hd cycle]));
        cycle @ cantbuild
      end else begin
        Printf.eprintf
          "%sERROR%s: the following packages are %smissing%s:\n"
          term.esc_red_text term.esc_end  term.esc_bold term.esc_end;
        List.iter (fun (name,_) ->
          Printf.eprintf "  - %s%s%s\n" term.esc_bold name term.esc_end
        ) missing_roots;
        List.map fst missing_roots @ cantbuild
      end
    in
    let cantbuild =
      if pj.project_incomplete = [||] then cantbuild
      else begin
        let additional =
          List.filter
            (fun pk -> pk.package_source_kind <> "meta"
              && not (List.mem pk.package_name cantbuild))
            (Array.to_list pj.project_incomplete)
        in
        if additional <> [] then
          Printf.eprintf
            "Additional packages %s can't be built.\n"
            (String.concat ", "
               (List.map (fun pk -> Printf.sprintf "%s%s%s"
                 term.esc_bold pk.package_name term.esc_end)
                  additional));
        List.map (fun pk -> pk.package_name) additional @ cantbuild
      end
    in
  *)
    cantbuild

let print_build_context = ref false
let do_init_project_building w p pj =
  let build_dir_basename = !build_dir_basename_arg in

  let build_dir_filename = (* absolute_filename *) build_dir_basename in

  let build_dir_filename =
    match !arch_arg with
    | Arch host -> Filename.concat build_dir_filename host
    | ArchNone -> build_dir_filename
  in

  BuildMisc.safe_mkdir build_dir_filename;

  BuildActions.time_step "Saving raw project info...";
  BuildOCP.save_project_state pj
    (FileGen.add_basename (FileGen.of_string build_dir_filename) "ocp.ocpx");
  BuildActions.time_step "   Done saving raw project info";

  let b =
    BuildEngineContext.create (FileGen.to_string p.project_dir)
      build_dir_filename in

  begin match p.cout.cout_ocamlbin with
  | None -> ()
  | Some ocamlbin ->
    ignore (BuildEngineContext.add_directory b ocamlbin);
  end;

  let bc = BuildGlobals.new_builder_context b in

  if !BuildGlobals.html_report_arg then BuildOCP.html_report pj;
  let packages = BuildOCamlRules.create w p.cin p.cout bc pj in

  if !print_build_context then
    BuildEngineDisplay.eprint_context b;
  (bc, packages, pj)

let load_initial_project pre_w p state config_state =

  List.iter (fun (name, enabled) ->
      if enabled then
        BuildOCP2Prims.with_feature name
      else
        BuildOCP2Prims.without_feature name
    ) !BuildActionConfigure.with_args;

  let w = BuildWarnings.empty_set () in
  do_load_project_files p.cin p.project_dir state config_state;

  (*    end; *)

  (* [ocp-build configure] stops here, so it will not scan
     for .ocp files at this point. Instead, it will be done the
     first time the project is compiled, because [root_files] is
     empty. *)

  if !configure_arg then save_project := true;

  (*
  if !save_project then begin
    Printf.fprintf stderr "Updating ocp-build.root\n%!";
    BuildOptions.must_save_project ()
  end;
  *)

  (*  if !conf_arg || !distrib_arg ||
      !autogen_arg then BuildMisc.clean_exit 0; *)

  let use_digests = p.cin.cin_digest in

  if use_digests then BuildMtime.use_digests true;

  BuildActions.time_step "Sorting packages...";
  let pj = BuildOCP.verify_packages w state in

  BuildActions.time_step "   Done sorting packages";

  (*
    do_reply_to_queries pj;
  *)

  if !query_global then begin
    Printf.eprintf "Error: reached query-global end point.\n%!";
    BuildMisc.clean_exit 0
  end;

  BuildOptions.maybe_save ();

  if verbose 1 && term.esc_ansi then begin
    let cantbuild = do_print_fancy_project_info pj in
    if cantbuild <> [] then begin
      BuildMisc.non_fatal_errors :=
        "Some package dependencies are missing" :: !BuildMisc.non_fatal_errors
    end
  end
  else
    do_print_project_info pj;

  let (bc, packages,  pj) = do_init_project_building w p pj in

  let package_map =
    let h = ref StringMap.empty in
    Array.iter (fun p ->
      let module P = (val p : Package) in
      h := StringMap.add P.name p !h;
    ) packages;
    !h
  in

  BuildActionsWarnings.print_pj_warnings p.project_dir (BuildWarnings.diff w pre_w);

  (bc, package_map, pj)

let load_installed_ocp = ref true
let arg_ocp_dirs = ref []


let do_read_env p =

  let cin = p.cin in
  let cout = p.cout in

  BuildOCamlConfig.set_global_config cout;

  (* Don't modify default values from now on, since they have been included
     in the default configuration ! *)

  let env_ocp_dirs = ref cin.cin_ocps_dirnames in
  let env_ocp_files = ref [] in
  let state = BuildOCP.init_packages () in

  begin
    match cout.cout_ocaml with
    | None -> Printf.eprintf "Compiler: no ocaml detected\n%!"
    | Some ocaml ->
      Printf.eprintf "Compiler: ocaml %s\n%!" ocaml.ocaml_version
  end;

  begin
    match cout.cout_ocamllib with
    None -> ()
    | Some ocamllib ->
      if cin.cin_ocps_in_ocamllib then begin
        env_ocp_dirs := ocamllib :: !env_ocp_dirs;
      end;

      BuildActions.time_step "Scanning env for .ocp files...";
      if !load_installed_ocp then
        List.iter (fun dir ->
          if verbose 3 then
            Printf.eprintf "Scanning installed .ocp files in %S\n%!" dir;
          let dir = FileGen.of_string dir in
          env_ocp_files := ( BuildOCP.scan_root dir) @ !env_ocp_files
        ) (!env_ocp_dirs @ cout.cout_meta_dirnames);
        List.iter (fun dir ->
          if verbose 3 then
            Printf.eprintf "Scanning installed .ocp files in %S\n%!" dir;
          let dir = FileGen.of_string dir in
          env_ocp_files := ( BuildOCP.scan_root dir) @ !env_ocp_files
        ) !arg_ocp_dirs;
      BuildActions.time_step "   Done scanning env for .ocp files";
      BuildActions.time_step "Loading METAs...";
      List.iter (fun dirname ->
        BuildOCamlMeta.load_META_files state ocamllib dirname
      ) cout.cout_meta_dirnames;
  end;

  BuildActions.time_step "   Done Loading METAs";

  BuildActions.time_step "Loading .ocp files from env...";

  let config = BuildOCP.generated_config () in
  let _nerrors = BuildOCP.load_ocp_files config state  !env_ocp_files in

  BuildActions.time_step "   Done Loading .ocp files from env";

  state, config.config_state

let chdir_to_project p =
  let dir = FileGen.to_string p.project_dir in
  if MinUnix.getcwd () <> dir then begin
    BuildMisc.chdir dir;
    Printf.fprintf stdout "ocp-build: Entering directory `%s'\n%!"
      (FileGen.to_string p.project_dir);
(* TODO: move at_exit to add_finally *)
    let final_handler_executed = ref false in
    let final_handler () =
      if not !final_handler_executed then begin
        final_handler_executed := true;
        Printf.printf
          "ocp-build: Leaving directory `%s'\n%!"
          (FileGen.to_string p.project_dir)
      end
    in
    (*    add_finally final_handler; *)
    at_exit final_handler
  end;
  ()

let print_env_arg = ref false

let init_env () =

  let w = BuildWarnings.empty_set () in
  let p = BuildActions.load_project w in
  let env_state, config_state = do_read_env p in
  let env_pj = BuildOCP.verify_packages w env_state in

  if !print_env_arg then begin
    BuildOCPPrinter.eprint_project "Environment packages" env_pj;
    exit 0;
  end;
  BuildActions.time_step "Environment read and checked.";
  (* TODO: we could check that all the packages are indeed installed ! *)

  BuildOCamlOCP2.init_env env_pj;

  BuildActionsWarnings.print_env_warnings p.project_dir w;

  (w, p, env_state, env_pj, config_state)

let action () =
  BuildActionsWarnings.set_default_is_always ();
  let (w, p, env_state, _env_pj, config_state) = init_env () in

  chdir_to_project p;

  let (_bc, _package_map, _pj) = load_initial_project w p
    (BuildOCP.copy_state env_state) config_state in
  ()

let arg_list =
  Arg.translate ~docs:"CHECK OPTIONS"
  [
  "-I", Arg.String (fun dir ->
    arg_ocp_dirs := !arg_ocp_dirs @ [ dir ]),
  "DIR Include files from DIR";

 "--print-loaded-env", Arg.Set
    print_env_arg,
  " Print the loaded environment and exit.";

   "--no-installed-ocp", Arg.Clear load_installed_ocp,
  " Do not load installed .ocp files";

  "--print-build-context", Arg.Set print_build_context,
  " Print full build context";

  "--with-help", Arg.Set print_queried_features_arg,
  " Print queried features and exit";
  ] @
    BuildActionsWarnings.arg_list

let subcommand = {
  Arg.cmd_name = command_name;
  cmd_man = [`P command_help];
  cmd_args =
    arg_list
    @ BuildActionConfigure.arg_with
    @ BuildActionsWarnings.arg_list
    @ Arg.translate_anon BuildArgs.arg_anon;
  cmd_doc = "Set the root of a project.";
  cmd_action = action;
}
