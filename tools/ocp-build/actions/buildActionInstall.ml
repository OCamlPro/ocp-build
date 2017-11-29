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

open OcpCompat

open BuildTypes
open BuildArgs
open BuildTerm
open BuildActions

open BuildOCamlInstall.TYPES

let uninstall_only = ref false
let print_only = ref false

let do_install dest_dir _install_what projects _package_map =

  let install_dirs = ref StringSet.empty in
  List.iter (fun p ->
    let module P = (val p : Package) in
    let list = P.install_dirs() in
    List.iter (fun s ->
      install_dirs := StringSet.add s !install_dirs) list;
  ) projects;

  let install_dirs = StringSet.to_list !install_dirs in

  let projects_to_install_set = ref StringSet.empty in
  let projects_to_install_list = ref [] in
  let add_to_install p =
    let module P = (val p : Package) in
    let lib = P.info in
    if P.to_install () &&
      not (StringSet.mem lib.lib_name !projects_to_install_set) then begin
      projects_to_install_set :=
        StringSet.add lib.lib_name !projects_to_install_set;
      projects_to_install_list := p :: !projects_to_install_list;
    end
  in

  List.iter add_to_install projects;

  if !print_only then Printf.printf "Packages to install:\n%!";
  let already_installed =

    let state = BuildUninstall.init dest_dir install_dirs
    in
    List.filter
      (fun p ->
        let module P = (val p : Package) in
         (*         let lib = P.info in *)
         (*         lib.lib_install && *)
        let already_installed = BuildUninstall.is_installed state P.name in
        if !print_only then
          Printf.printf "  %s %s\n%!" P.name
            (if already_installed then " (uninstall first)" else "");
        already_installed
      )
      !projects_to_install_list
  in
  if !print_only then exit 0;

  let bold s =
    if term.esc_ansi then Printf.sprintf "\027[1m%s\027[m" s else s
  in
  if already_installed <> [] then begin
    let names =  String.concat ", " (List.map (fun p ->
      let module P = (val p : Package) in
      bold P.name) already_installed) in
    if !BuildArgs.auto_uninstall then begin
      Printf.printf "Packages %s are already installed, removing first...\n"
        names;
      let state =
        BuildUninstall.init dest_dir install_dirs
      in
      List.iter
        (fun p ->
          let module P = (val p : Package) in
          BuildUninstall.uninstall state P.name
        )
        already_installed;
      BuildUninstall.finish state
    end else begin
      Printf.eprintf "Error: Packages %s are already installed.\n%!" names;
      BuildMisc.clean_exit 2
    end;
  end;

  if not !uninstall_only then

    let install_errors = ref 0 in
    let install_ok = ref 0 in

    List.iter (fun p ->
      let module P = (val p : Package) in
        (*        let lib = P.info in *)
      P.install ();
      incr install_ok
    )
      !projects_to_install_list;
    if !install_errors > 0 then begin
      if !install_ok = 0 then
        Printf.eprintf "Install completely failed\n%!"
      else
        Printf.eprintf
          "Install partially failed: %d/%d packages not installed"
          !install_errors (!install_errors + !install_ok);
      BuildMisc.clean_exit 2
    end

let arg_list =
  Arg.translate ~docs:"INSTALL OPTIONS"
      [
  "-install-bundle", Arg.String (fun _s ->
    Printf.eprintf "Warning: option -install-bundle is obsolete\n%!"
    ),
  "BUNDLE Install a bundle packages to uninstall all\n  packages at once";

        "--uninstall-only", Arg.Set uninstall_only,
  " Only uninstall packages supposed to be install";

        "--print-only", Arg.Set print_only,
  " Only compute and print the list of actions to perform";

      ]


let action () =
  BuildActionMake.make_build_targets := true;
  let (p, _bc, projects, package_map) = BuildActionMake.do_build () in

  let install_where = BuildOCamlInstall.install_where p.cin p.cout in
  let install_what = BuildOCamlInstall.install_what () in
  do_install install_where.install_destdir
    install_what projects package_map;
  ()



let subcommand = {
  Arg.cmd_name = "install";
  cmd_man = [`P "Install the project."];
  cmd_args = arg_list
             @ BuildActionMake.arg_list
             @ Arg.translate_anon BuildArgs.arg_anon;
  cmd_doc = "Install the project.";
  cmd_action = action;
}
