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

open Ezcmd.Modules
open BuildValue.TYPES
open BuildEngineTypes
open BuildTerm

let number_of_cores () =
  let ncores = ref 0 in
  (* Compute number of cores, including hyper-threading, on a linux machine *)
  begin try
          FileString.iter_lines (fun line ->
            if OcpString.starts_with line ~prefix:"processor" then incr ncores
          ) "/proc/cpuinfo"
    with _ -> ()
  end;
  !ncores

let targets_arg = ref []
let delete_orphans_arg = ref DeleteOrphanFiles
let build_dir_basename_arg = ref "_build"
let njobs_arg = ref (number_of_cores ())

let cmd_name = "do"
let cmd_args =
  let docs = "BUILD OPTIONS" in
  [
    ["j"; "jobs"],
    Arg.Set_int njobs_arg,
    Ezcmd.info ~docv:"NJOBS"
               ~docs
               ~env:(Ezcmd.env "COP_NJOBS")
               (Printf.sprintf "Set the level of parallelism (current %d)"
                               !njobs_arg);

    [],
    Arg.Anons (fun targets -> targets_arg := targets),
    Ezcmd.info ~docs ~docv:"TARGETS" "";
  ]

let cmd_man = [
    `S "DESCRIPTION";
    `P "Build the given targets, or build@ if none is specified"
  ]

let cmd_doc = "Build targets"

let add_prims root =

  CopEval.add_prim
    "root" [ "Return the root of the workspace" ]
    (fun _loc _state _config _args -> VString(root, StringRaw) );

  CopEval.add_prim
    "new_switch"
    [ "Create a build switch: new_switch(name:string)" ]
    (fun loc c _config args ->
      match args with
      | [ VString (sw_name,_) ] ->
         CopSwitch.parse_switch c sw_name BuildValue.empty_env;
         VBool true
      | [ VString (sw_name,_); VObject sw_env ] ->
         CopSwitch.parse_switch c sw_name sw_env;
         VBool true
      | _ ->
         BuildOCP2Prims.raise_bad_arity
           loc
           "new_switch(name:string[,env])" 1 args
    );

  CopEval.add_prim
    "new_package"
    [ "Create a new package: new_package(name, requires, description)" ]
    (fun loc c config args ->
      if CopSwitch.has_switch() then
        try
          match args with
          | [ VString(name, _) ;
              pk_info ;
              pk_description ] ->
             CopEval.add_project c loc name config pk_info pk_description;
             VBool true
          | _ -> raise Not_found
        with
        | Var_not_found var ->
          BuildOCP2Prims.raise_bad_arity
            loc
            (Printf.sprintf "new_package(): missing variable %S" var)
            3 args
        | _ ->
          BuildOCP2Prims.raise_bad_arity
            loc
            "new_package(name:string, requires:list, description)" 3 args
      else
        BuildOCP2Prims.ocp2_raise
          loc
          "invalid-call"
          (VString ("new_package() called outside of switch", StringRaw))
    )

let start_engine b build_targets =
  if build_targets <> [] then begin
      let t0 = MinUnix.gettimeofday () in
      begin

        try
          BuildEngine.init b build_targets
        with BuildEngine.MissingSourceWithNoBuildingRule (r, filename) ->
          BuildEngineDisplay.print_loc r.rule_loc;
          Printf.eprintf "Error: in project \"%s\", the source filename\n"
                         r.rule_loc.loc_package.package_package;
          Printf.eprintf "\t\"%s\" does not exist\n" filename;
          BuildEngineDisplay.print_rule r;
          BuildMisc.clean_exit 2
      end;

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
            "Warning: deleted %d orphan files in %s\n"
            (-orphans) !build_dir_basename_arg;

      BuildEngine.parallel_loop b !njobs_arg;

      let errors =
        List.map BuildEngineDisplay.strings_of_fatal_error
                 (BuildEngine.fatal_errors b) @
          List.map BuildEngineDisplay.strings_of_error
                   (BuildEngineDisplay.errors b)
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
  Printf.eprintf "%!"

let cmd_action () =

  let root, workspace = CopArgs.lookup_root () in
  let _project_files, module_files = CopWorkspace.scan_workspace root in

  let state = CopEval.init_state () in
  let c, config = CopEval.init_workspace state module_files in

  let b =
    BuildEngineContext.create root
      (Filename.concat root "_build")
  in

  add_prims root;

  let _config = CopEval.eval_file c config
                                  (Filename.concat root workspace)
  in

  if not ( CopSwitch.has_switch () ) then begin
      Printf.eprintf "Warning: no switch defined in '%s', using 'default'.\n"
                     workspace;
         CopSwitch.parse_switch c "default" BuildValue.empty_env;
    end;


  (* TODO: start the engine and build *)
  start_engine b []

let cmd = {
    Arg.cmd_name;
    cmd_args;
    cmd_doc;
    cmd_man;
    cmd_action;
  }
