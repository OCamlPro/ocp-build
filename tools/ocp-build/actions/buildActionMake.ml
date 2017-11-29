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

(* TODO: we should save the version of ocaml used to build a project,
   so that we can detect changes and ask for a clean before building.
   Can we access the magic used by every compiler ? (we can compile an
   empty file in bytecode and native code) We could cache this
   information using the uniq identifier of the executable (would not
   work with wrappers).
*)

(* TODO
   We could force packages with missing dependencies to still be compiaboutled,
   since it is still possible that these missing dependencies arbue not used
   in a particular compilation scheme.
*)

open Ezcmd.Modules

let command_name = "make"
let command_help = {|
ocp-build make [MAKE_OPTIONS] [CONFIGURE_OPTIONS] [CHECK_OPTIONS]

Build the project.
|}

open OcpCompat

open SimpleConfig

open BuildOCamlConfig.TYPES
open BuildEngineTypes
open BuildOCPTypes

open BuildTypes
open BuildGlobals
open BuildOptions
open BuildArgs
open BuildTerm
open BuildActions
open BuildValue.TYPES
open BuildUninstall.TYPES
open BuildOCamlInstall.TYPES

let _verbose = OcpDebug.verbose_function ["B"; "BuildActionMake"]
  (*
  if
    !build_max ||
      (!targets_arg <> []
       && List.for_all
         (fun (name,_) -> not (List.mem name !targets_arg)) missing
       && List.for_all
         (fun pk -> not (List.mem pk.package_name !targets_arg))
         (Array.to_list pj.project_incomplete))
  then []
  else
  *)

let max_stage = ref 20

let build_max = ref false

let make_build_targets = ref false
let make_doc_targets = ref false
let make_test_targets = ref false

let print_installed install_where =
  Printf.printf "Installed packages:\n";
  List.iter (fun un ->
    Printf.printf "\t%s . %s (%s)\n%!"
      un.un_name un.un_version un.un_type;
    Printf.printf "\t\tin %s\n%!" un.un_directory;
  ) (BuildUninstall.list_installed install_where);
  ()

let move_to_project = ref true

  (*
let finally_do = ref []
let add_finally action =
  finally_do := action :: !finally_do
  *)




let rec do_compile stage p ncores env_state arg_targets pre_w
                   config_state =

  let (bc, package_map, _pj) =
    BuildActionCheck.load_initial_project pre_w p
                                          (BuildOCP.copy_state env_state) config_state in

  if !configure_arg then BuildMisc.clean_exit 0;

  if !clean_arg then begin
      Printf.eprintf "Removing build target directory\n%!";

      BuildActions.delete_file_or_directory !build_dir_basename_arg;
      BuildMisc.clean_exit 0;
    end;

  bc.build_context.stop_on_error_arg <- !stop_on_error_arg;

  let projects =
    (* build the list of projects considered by the current command *)
    let projects = ref [] in
    match arg_targets with
      [] ->
      StringMap.iter (fun _ pj ->
          let module P = (val pj : Package) in
          if not (P.pre_installed ()) then
            projects := pj :: !projects) package_map;
      !projects
    | list ->
       List.iter (fun name ->
           try
             let pj = StringMap.find name package_map in
             projects := pj :: !projects
           with Not_found ->
             Printf.eprintf
               "Error: Could not find target project %s\n%!" name;
             BuildMisc.clean_exit 2
         ) list;
       !projects
  in

  let b = bc.build_context in

  (* build the list of targets *)
  let build_targets = ref [] in
  let map = ref StringMap.empty in
  let rec add_project_targets
            (make_build_targets, make_doc_targets, make_test_targets)
            p =
    let module P = (val p : Package) in
    let lib = P.info in
    if not (StringMap.mem lib.lib_name !map) then begin

        (* prevent second pass *)
        map := StringMap.add lib.lib_name lib !map;

        let add_project_targets pj =
          let p = StringMap.find pj.lib_name package_map in
          add_project_targets (true, false, false) p
        in
        if make_build_targets then begin
            let { targets; depends } = P.build_targets () in
          build_targets := targets @ !build_targets;
            List.iter add_project_targets depends
          end;
        if make_doc_targets then begin
            let { targets; depends } = P.doc_targets () in
            build_targets := targets @ !build_targets;
            List.iter add_project_targets depends
          end;
        if make_test_targets then begin
            let { targets; depends } = P.test_targets () in
            build_targets := targets @ !build_targets;
            List.iter add_project_targets depends
          end;
      end
  in
  List.iter (add_project_targets
               (!make_build_targets, !make_doc_targets, !make_test_targets)
            ) projects;

  if !build_targets = [] then begin
      Printf.eprintf "Error: project contains no targets\n%!";
      Printf.eprintf "\tAre your .ocp files empty ?\n%!";
      BuildMisc.clean_exit 2
    end;

  (*
        List.iter (fun s ->
        Printf.eprintf "TARGET %S\n%!" (File.to_string s.file_file)
        ) !targets;
   *)



  if !build_targets <> [] then begin
      time_step "Initializing build engine...";
      begin

        try
          BuildEngine.init b !build_targets
        with BuildEngine.MissingSourceWithNoBuildingRule (r, filename) ->
          BuildEngineDisplay.print_loc r.rule_loc;
          Printf.eprintf "Error: in project \"%s\", the source filename\n"
                         r.rule_loc.loc_package.package_package;
          Printf.eprintf "\t\"%s\" does not exist\n" filename;
          BuildEngineDisplay.print_rule r;
          BuildMisc.clean_exit 2
      end;
      time_step "   Build Engine Initialized";
      time_step "Checking remaining artefacts...";
      let orphans = BuildEngine.sanitize b !delete_orphans_arg
                                         (fun basename ->
                                           match basename with
                                             "_tests" | "_reports" -> true
                                             | _ -> false)
      in
      if orphans > 0 then begin
          Printf.eprintf "Error: found %d orphan files in %s. You must remove them.\n" orphans !build_dir_basename_arg;
          Printf.eprintf "\n";
          Printf.eprintf "   You can add the -sanitize argument to automatically remove\n";
          Printf.eprintf "   orphan files\n";
          Printf.eprintf "\n";
          BuildMisc.clean_exit 2;
        end else
        if orphans < 0 then
          Printf.eprintf
            "Warning: deleted %d orphan files in %s\n" (-orphans) !build_dir_basename_arg;
      time_step "   Done sanitizing";

      time_step "Building packages...";
      BuildEngine.parallel_loop b ncores;
      time_step "   Done building packages";

      let errors =
        List.map BuildEngineDisplay.strings_of_fatal_error (BuildEngine.fatal_errors b) @
          List.map BuildEngineDisplay.strings_of_error (BuildEngineDisplay.errors b)
      in
      let t1 = MinUnix.gettimeofday () in

      let nerrors = List.length errors in
      Printf.eprintf
        "%s in %.2fs. %d jobs (parallelism %.1fx), %d files generated.\n%!"
        (if errors = [] then
           if term.esc_ansi then
             Printf.sprintf "%sBuild Successful%s"
                            term.esc_green_text term.esc_end
           else "Build Successful"
         else
           Printf.sprintf "%s%d error%s%s" term.esc_red_text
                          nerrors
                          (if nerrors > 1 then "s" else "")
                          term.esc_end)
        (t1 -. t0)
        b.stats_command_executed
        (b.stats_total_time /. (t1 -. t0))
        b.stats_files_generated;
      if errors <> [] (* && not (verbose 1 && term.esc_ansi) *) then begin
          Printf.eprintf "Error log:\n";
          List.iter (fun lines ->
              Printf.eprintf "Error:\n";
              List.iter (fun line ->
                  Printf.eprintf "%s\n" line
                ) lines
            ) errors;
        end;
      if errors <> [] then BuildMisc.clean_exit 2
    end;
  Printf.eprintf "%!";
  if b.build_should_restart then
    if stage = !max_stage then begin
        Printf.eprintf "Error: build restarted too many times (%d times). Aborting\n%!" stage;
        BuildMisc.clean_exit 2
      end else begin
        Printf.eprintf "Some configuration files were changed. Restarting build\n%!";

        do_compile (stage+1) p ncores  env_state arg_targets pre_w config_state
      end else
    (p, bc, projects, package_map)

let get_ncores cin =
  let ncores = cin.cin_njobs in
  if ncores < 1 then
    BuildConfig.number_of_cores () + 1
  else
    ncores


(* Also called from BuildActionTests.action () *)
let do_build () =

  let (env_w, p, env_state, _env_pj, config_state) =
    BuildActionCheck.init_env () in

  if !query_global then move_to_project := false;


  if !list_installed_arg then begin
    let state =
      let where = BuildOCamlInstall.install_where p.cin p.cout in
      BuildUninstall.init where.install_destdir where.install_libdirs
    in
    print_installed state;
    BuildMisc.clean_exit 0
  end;

  let targets = List.rev !targets_arg in
  if !uninstall_arg && targets <> [] then begin
    let state =
      let where = BuildOCamlInstall.install_where p.cin p.cout in
      BuildUninstall.init where.install_destdir where.install_libdirs
    in
    List.iter (BuildUninstall.uninstall state) targets;
    BuildUninstall.finish state;
    BuildMisc.clean_exit 0
  end;

  begin match !query_install_dir with
      None -> ()
    | Some package ->
      let state =
        let where = BuildOCamlInstall.install_where p.cin p.cout in
        BuildUninstall.init where.install_destdir where.install_libdirs
      in
      List.iter (fun un ->
        if un.un_name = package then begin
          Printf.printf "%s\n%!" un.un_directory;
          BuildMisc.clean_exit 0
        end
      ) (BuildUninstall.list_installed state);
      Printf.eprintf "Package %S is not installed\n%!" package;
      BuildMisc.clean_exit 2
  end;


  BuildActionCheck.chdir_to_project p;

  do_compile 0 p (get_ncores p.cin) env_state targets env_w config_state


let action () =
(* Nothing specified, make build targets: *)
  if not !make_doc_targets && not !make_test_targets then make_build_targets := true;
(* Test targets require build targets ? *)
  if !make_test_targets then make_build_targets := true;
  if !make_doc_targets then make_build_targets := true;

  let (_p, _b, _projects, _package_map) = do_build () in
  ()

let arg_list =
  [
  (* This option should be shared with -install and -tests, no ? *)
  "--arch", Arg.String (fun s ->
    arch_arg := Arch ("_other_archs/" ^ s)),
  "ARCH Set arch sub-directory of _obuild";

  "--max", Arg.Set build_max, " Build as many packages as possible";

  "--max-stage", Arg.Int (fun n -> max_stage := n),
  "NUM Maximal number of times compilation can be restarted";
  "--print-loaded-ocp-files", Arg.Set
    BuildOCP.print_loaded_ocp_files,
  " Print loaded ocp files";
 "--print-package-deps", Arg.Set
    BuildOCP.print_package_deps,
 " Print package dependencies";
 "---print-missing", Arg.Set
   BuildOCP.print_missing_deps, " Print missing dependencies";
 "--print-conflicts", Arg.Set
    print_conflicts_arg,
 " Print conflicts between package definitions";

  "--doc", Arg.Set make_doc_targets, " Make doc targets";
  "--test", Arg.Set make_test_targets, " Make tests targets";
  "--build", Arg.Set make_build_targets, " Make build targets";

  "--continue-on-ocp-error", Arg.Set BuildOCP.continue_on_ocp_error, " Continue after finding a syntax error in an ocp file";

  "--init", Arg.Unit (fun () ->
    BuildActionInit.action ();
    exit 0),
  " Set the root of a project in the current directory and exit.";

  "--version", Arg.Unit (fun () ->
    Printf.printf "%s\n%!" BuildVersion.version;
    BuildMisc.clean_exit 0
  ),
  " Print version information";

  "--disable", Arg.String BuildOCP.conf_add_disabled_package,
  "PKG@DIR Disable package PKG installed in DIR";

  "--html-report", Arg.Set BuildGlobals.html_report_arg,
  " Create an HTML report in _obuild/_reports";

  "--dot-report", Arg.Set BuildGlobals.dot_report_arg,
  " Create a DOT report in _obuild/_reports";

  "--replay-script", Arg.Set BuildEngineReport.output_replay_script,
  " Generates _obuild/_reports/build-replay.sh";

  "--graph-report", Arg.Set BuildEngineReport.output_graph_report,
  " Generates _obuild/_reports/graph.dot";

  ]
  @ arg_list1


let add_synomyms arg_list1 synonyms =
  arg_list1 @ List.map (fun (s1, s2) ->
    let rec iter list =
      match list with
      [] -> assert false
      | (s, action, help) :: _tail when s = s2 -> (s1, action, help)
      | _ :: tail -> iter tail
    in
    iter arg_list1
  ) synonyms

let arg_list = add_synomyms arg_list
    [ "-v", "-verbosity";
      "-j", "-njobs";
    ]

let arg_list = Arg.translate ~docs:"BUILD OPTIONS" arg_list

let subcommand = {
  Arg.cmd_name = command_name;
  cmd_man = [ `S command_help ];
  cmd_args = arg_list
             @ BuildActionInit.arg_list
             @ BuildActionConfigure.arg_with
             @ BuildActionCheck.arg_list
             @ Arg.translate_anon arg_anon;
  cmd_doc = "Build";
  cmd_action = action;
}

let old_subcommand =
  {
  Arg.cmd_name = "build";
  cmd_man = [`S "(deprecated, use 'ocp-build make' subcommand)"];
  cmd_args = arg_list
             @ Arg.translate_anon arg_anon;
  cmd_doc = "Build";
  cmd_action = action;
}
