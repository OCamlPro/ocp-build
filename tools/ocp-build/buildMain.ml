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

open OcpCompat

open BuildActions
open BuildGlobals
open BuildArgs

let verbose = OcpDebug.verbose_function ["B";  "BuildMain" ]


let finally () =
  (*  List.iter (fun action -> action ()) !BuildActionBuild.finally_do; *)
  time_step "End of execution";

  if !time_arg then begin
    Printf.printf "Time schedule:\n";
    List.iter (fun (msg, t1) ->
      Printf.printf "  %.2fs\t%s\n%!" (t1 -. t0) msg
    ) (List.rev !time_steps);
    Printf.printf "%!";
  end;
  ()

let subcommands =  [
    BuildActionInit.subcommand;
    BuildActionCheck.subcommand;
    BuildActionConfigure.subcommand;
    BuildActionMake.subcommand;
    BuildActionTests.subcommand;
    BuildActionInstall.subcommand;
    BuildActionUninstall.subcommand;
    BuildActionClean.subcommand;
    BuildActionMake.old_subcommand;

    BuildActionPrefs.subcommand;
    BuildActionQuery.subcommand;
  ]


let _ =

  Printexc.record_backtrace true;

  (*
  begin match initial_verbosity with None -> () | Some v ->
    DebugVerbosity.increase_verbosity "B"  v end;
   *)

  BuildOCamlPlugin.init "UNKNOWN";
  let argv = Array.copy Sys.argv in
  for i = 0 to Array.length argv - 1 do
    match argv.(i) with
    | "-version" -> argv.(i) <- "--version"
    | "-scan" -> argv.(i) <- "--scan"
    | "-init" -> argv.(i) <- "--init"
    | "-install-lib" -> argv.(i) <- "--install-lib"
    | "-install-bin" -> argv.(i) <- "--install-bin"
    | "-install-meta" -> argv.(i) <- "--install-meta"
    | _ -> ()
  done;
  try
    let () = Ezcmd.main_with_subcommands
               ~name:"ocp-build"
               ~doc:"OCaml Highly-Parallel Build System"
               ~man: []
               ~argv
               ~default: "make"
               subcommands in
    ()

  with
  | BuildMisc.ExitStatus n ->
    let exit_status =
      if n = 0 then
        match !BuildMisc.non_fatal_errors with
        | [] -> 0
        | msgs ->
          Printf.eprintf "Work finished after non-fatal errors:\n%!";
          List.iter (fun msg ->
            Printf.eprintf "   %s\n%!" msg) (List.rev msgs);
          2
      else n
    in
    Pervasives.exit exit_status
  | e ->
    let backtrace = Printexc.get_backtrace () in
    Printf.fprintf stderr "ocp-build: Fatal Exception %s\n%s\n%!"
      (Printexc.to_string e) backtrace;
    raise e
