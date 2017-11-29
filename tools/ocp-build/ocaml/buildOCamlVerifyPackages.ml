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

(*
  This module does several things in [verify_packages]:
  * it checks which packages have unmet dependencies, and set
     package_disabled=Some reason
  * it computes the dependencies in package_requires_list
  * it disables bytecode or native code compilation when compiling in one of
      them is not possible because of missing dependencies
  * it computes OCaml dependencies in opk_requires, each dependency being
      tagged as dep_syntax, dep_link or dep_optional
  * it propagates link dependencies to requires
  * it prefers sources packages to install packages
*)

let verbose = OcpDebug.verbose_function [ "B"; "BP"; "BuildOCamlCheckPackages"]
let print_incomplete_packages = ref false

module Warnings = struct

  let w_MissingDirectory w (dirname, name, filename) =
    BuildWarnings.wprintf w
      "Warning: inexistent package directory:\n\
            \  %S\n\
            \  Disabling package %S.\n\
            \    (from %S)\n"
      dirname name filename

  let w_MissingDependency w opk dep_name =
    BuildWarnings.wprintf w
      "Warning: missing package %S:\n\
            \  Disabling package %S.\n\
            \    (from %S)\n"
      dep_name opk.opk_name opk.opk_package.package_filename

  let w_InstalledOnSourceDependency w opk dep_name =
    BuildWarnings.wprintf w
      "Warning: incorrect installed-on-source dependency on %S:\n\
            \  Disabling package %S.\n\
            \    (from %S)\n"
      dep_name opk.opk_name opk.opk_package.package_filename

  let w_ConflictingInstalledPackages w opk1 opk2 =
    BuildWarnings.wprintf w
      "Warning: installed package %S has conflicting directories:\n"
      opk1.opk_name;
    BuildWarnings.wprintf w "   first: %s@%s\n" opk2.opk_name opk2.opk_dirname;
    BuildWarnings.wprintf w "     (from %s)\n" opk2.opk_package.package_filename;
    BuildWarnings.wprintf w "   second: %s@%s\n" opk1.opk_name opk1.opk_dirname;
    BuildWarnings.wprintf w "     (from %s)\n" opk1.opk_package.package_filename;
    BuildWarnings.wprintf w
      "Use the --disable-package PKG@DIR to disable one of them.\n%!";
    BuildWarnings.wprintf w "Choosing %S.\n%!" opk2.opk_name

end

let print_deps msg opk =
  Printf.eprintf "%s: Project %s depends on:\n" msg opk.opk_name;
  Printf.eprintf "  (from %s)\n" opk.opk_dirname;
  List.iter (fun dep ->
    let pd = dep.dep_project in
    Printf.eprintf "\t%s %s%s%s%s\n%!"
      (BuildOCP.string_of_package_type pd.opk_kind)
      pd.opk_name
      (if dep.dep_link then "(link)" else "")
      (if dep.dep_syntax then "(syntax)" else "")
      (if dep.dep_optional then "(optional)" else "")
  ) opk.opk_requires

let is_better_installed_package w opk1 opk2 =
  if opk1.opk_dirname = opk2.opk_dirname then
    match
      opk1.opk_package.package_source_kind,
      opk2.opk_package.package_source_kind
    with
    | "ocp", _ -> true
    | _ -> false

  else begin
    Warnings.w_ConflictingInstalledPackages w opk1 opk2;
    false
  end

let is_better_package w opk1 opk2 =
  match opk1.opk_installed, opk2.opk_installed with
  | false, true -> true (* sources is better than installed *)
  | true, false -> false (* sources is better than installed *)
  | true, true -> is_better_installed_package w opk1 opk2
  | false, false ->
    Printf.eprintf
      "Error: package %S is defined twice in the project sources:\n"
      opk1.opk_name;
    Printf.eprintf "   first: %s\n" opk2.opk_dirname;
    Printf.eprintf "     (from %s)\n" opk2.opk_package.package_filename;
    Printf.eprintf "   second: %s\n" opk1.opk_dirname;
    Printf.eprintf "     (from %s)\n" opk1.opk_package.package_filename;
    Printf.eprintf "Aborting.\n%!";
    exit 2

let get_uniq_ocaml_packages w state =

  let packages = ref StringMap.empty in

  IntMap.iter (fun _i pk ->
    match pk.package_plugin with
    | OCamlPackage opk when not (package_disabled pk) ->

      if not ( BuildMisc.exists_as_directory opk.opk_dirname ) then begin

          Warnings.w_MissingDirectory w
            ( opk.opk_dirname,
              opk.opk_name,
              pk.package_filename);
          pk.package_disabled <- Some "Directory does not exist";

        end else begin

          try

            let opk2 = StringMap.find opk.opk_name !packages in

            if is_better_package w opk opk2 then begin
              (* disable the installed package, and choose this one *)
              opk2.opk_package.package_disabled <-
                Some "Superseeded by better package";
              raise Not_found
              end else
              opk.opk_package.package_disabled <-
                Some "Superseeded by better package"

          with Not_found ->
            packages := StringMap.add opk.opk_name opk !packages;

        end

    | _ -> ()
  ) (BuildOCP.get_packages state);
  !packages


(* Add a dependency, adding link info if needed. *)
let add_dep dep_project dep_link dep_syntax dep_options map =
  try
    let dep = StringMap.find dep_project.opk_name map in
    dep.dep_link <- dep.dep_link || dep_link;
    dep.dep_syntax <- dep.dep_syntax || dep_syntax;
    map
  with Not_found ->
    let dep =
      {
        dep_project;
        dep_link;
        dep_syntax;
        dep_optional = false;
        dep_options;
      } in
    StringMap.add dep_project.opk_name dep map

let rec disable_package warning w packages opk dep_name =
  warning w opk dep_name;
  opk.opk_package.package_disabled <-
    Some (Printf.sprintf "Missing dependency %S" dep_name);
  packages := StringMap.remove opk.opk_name !packages;
  StringMap.iter (fun _ dep ->
    disable_package
      Warnings.w_MissingDependency w packages dep.dep_project opk.opk_name
  ) opk.opk_usedby_map;
  opk.opk_usedby_map <- StringMap.empty

let build_dependency_graph w packages =
  let packages = ref packages in

  StringMap.iter (fun _ opk ->
    let requires =
      try BuildValue.prop_list (BuildValue.get opk.opk_options "requires")
      with Var_not_found _ -> []
    in

    (*    Printf.eprintf "Packages  %s.requires = %s\n%!"
          opk.opk_name
          (String.concat " " (List.map fst requires)); *)
    List.iter (fun (dep_name, options) ->
      try
        let opk2 = StringMap.find dep_name !packages in

        (* An installed package should never depend on a source package ! *)
        if opk.opk_installed && not opk2.opk_installed then begin
          let optional =
            BuildValue.get_bool_with_default [options] "optional" false in
          if not optional then
            disable_package Warnings.w_InstalledOnSourceDependency
              w packages opk dep_name
        end else

          let tolink = BuildValue.get_bool_with_default [options] "tolink"
            opk2.opk_tolink in
          let syntax = BuildValue.get_bool_with_default [options] "syntax"
            opk2.opk_syntax in

          opk.opk_requires_map <-
            add_dep opk2 tolink syntax options opk.opk_requires_map;
          opk2.opk_usedby_map <-
            add_dep opk tolink syntax options opk2.opk_usedby_map;

      with Not_found ->
        let optional =
          BuildValue.get_bool_with_default [options] "optional" false in
        if not optional then
          disable_package Warnings.w_MissingDependency w packages opk dep_name
    ) requires

  ) !packages;

  !packages

(* The main function ! *)
let verify_packages w state =

  (* remove duplicates packages and packages without corresponding
     directory *)
  let packages = get_uniq_ocaml_packages w state in

  if !BuildGlobals.dot_report_arg then
    BuildOCamlDotReport.report packages;

  let packages =  build_dependency_graph w packages in

  StringMap.iter (fun _ opk ->
    if !BuildOCP.print_package_deps || verbose 5 then
      print_deps "BEFORE update_deps" opk) packages;


  (* (1) we need to propagate link dependencies in the graph *)


  (* Now, we can create an internal order on dependencies *)
  StringMap.iter (fun _ opk -> opk.opk_id <- -1) packages;

  let opk_id = ref 0 in
  let sorted_packages = ref [] in
  let rec set_id opk _trace =
    if opk.opk_id = -2 then begin
      Printf.eprintf "CYCLE !\n%!"; (* we should use the trace ! *)
      exit 2;
    end;
    if opk.opk_id < 0 then begin
      opk.opk_id <- -2;
      StringMap.iter (fun _ dep ->
        set_id dep.dep_project [opk];
      ) opk.opk_requires_map;
      opk.opk_id <- !opk_id;
      incr opk_id;
      sorted_packages := opk :: !sorted_packages;
    end
  in
  StringMap.iter (fun _ opk -> set_id opk []) packages;
  let sorted_packages = List.rev !sorted_packages in

  (* Add transitively [dep_link] dependencies *)
  List.iter (fun opk ->
    StringMap.iter (fun _ dep ->
      if dep.dep_link then begin
        StringMap.iter (fun _ dep ->
          if dep.dep_link then
            opk.opk_requires_map <-
              add_dep
              dep.dep_project
              dep.dep_link
              dep.dep_syntax
              dep.dep_options
              opk.opk_requires_map
        ) dep.dep_project.opk_requires_map;
      end
    ) opk.opk_requires_map;
  ) sorted_packages;

  (* Sort [opk.opk_requires] and
     [opk.opk_package.package_requires_list].  We also take this
     opportunity to propagate [has_asm] and [has_byte] information, so
     that we won't try to build something if the corresponding
     dependency does not exist in bytecode or native.
  *)
  StringMap.iter (fun _ opk ->
    opk.opk_requires <- [];
    StringMap.iter (fun _ dep ->
      opk.opk_requires <- dep :: opk.opk_requires;
      if dep.dep_link then begin
        if opk.opk_has_asm && not dep.dep_project.opk_has_asm then
          opk.opk_has_asm <- false;
        if opk.opk_has_byte && not dep.dep_project.opk_has_byte then
          opk.opk_has_byte <- false;
      end
    ) opk.opk_requires_map;
    opk.opk_requires_map <- StringMap.empty;
    opk.opk_usedby_map <- StringMap.empty;

    opk.opk_requires <- List.sort (fun dep1 dep2 ->
      compare dep1.dep_project.opk_id dep2.dep_project.opk_id)
      opk.opk_requires;
    opk.opk_package.package_requires_list <-
      List.map (fun dep -> dep.dep_project.opk_package) opk.opk_requires
  ) packages;

  StringMap.iter (fun _ opk ->
    if !BuildOCP.print_package_deps || verbose 5 then
      print_deps "AFTER update_deps" opk) packages;


  ()
