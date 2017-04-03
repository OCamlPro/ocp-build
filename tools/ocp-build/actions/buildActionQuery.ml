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

(* ocp-build install [OPTIONS]

  Set the options of the user preference file.

*)








open StdlibArg
open BuildOCPTypes
open BuildTypes
open BuildArgs

open BuildActions


let query_libdir = ref []
let query_package = ref []
let list_arg = ref false

let arg_list =
  BuildOptions.merge
    [
      [
        "-libdir", Arg.String (fun s ->
          query_libdir := s :: !query_libdir
        ),
        "PACKAGE Query libdir of PACKAGE";
        "-has", Arg.String (fun s ->
          query_package := s :: !query_package
        ),
        "PACKAGE Query if PACKAGE is available";
        "-list", Arg.Set list_arg, " List packages from env";
      ];
      BuildActionBuild.arg_list
    ]



let do_reply_to_queries pj =

  List.iter (fun p ->
    Array.iter (fun pk ->
      if pk.package_name = p then begin
        Printf.printf "%s\n" pk.package_dirname;
        BuildMisc.clean_exit 0
      end
    ) pj.project_sorted;
    Printf.eprintf "Error: no package %S\n%!" p;
    BuildMisc.clean_exit 2
  ) !query_libdir;

  List.iter (fun p ->
    try
      Array.iter (fun pk ->
        if pk.package_name = p then raise Exit
      ) pj.project_sorted;
      Printf.eprintf "Error: no package %S\n%!" p;
      BuildMisc.clean_exit 2
    with Exit ->
      Printf.printf "Package %S is present\n%!" p
  ) !query_package

let do_list_packages pj =
  List.iter (fun pk ->
    Printf.printf "  %s (%s in %s)\n" pk.package_name
      (match pk.package_type with
       | LibraryPackage -> "library"
       | ProgramPackage -> "program"
       | TestPackage -> "test"
       | ObjectsPackage -> "objects"
       | SyntaxPackage -> "syntax"
       | RulesPackage -> "rules"
      )
      pk.package_dirname;
    Printf.printf "    ";
(*    List.iter (fun dep ->
      Printf.printf "%s %s "
        dep.dep_project.package_name
        (match dep.dep_link, dep.dep_syntax with
         | true, false -> ""
         | false, false -> "(none)"
         | false, true -> "(syntax)"
         | true, true -> "(link+syntax)")
      ) pk.package_requires_list; *)
    Printf.printf "\n";
  ) (List.sort (fun pk1 pk2 -> compare pk1.package_name pk2.package_name)
      (Array.to_list pj.project_sorted));
  Printf.printf "DISABLED:\n%!";
  Array.iter (fun pk ->
    Printf.printf "  %s (in %s)\n" pk.package_name pk.package_dirname
  ) pj.project_disabled;
  (*
  Printf.printf "MISSING:\n%!";
  List.iter (fun (package_name, missed_by) ->
    Printf.printf "  %s missed by:\n" package_name;
    List.iter (fun pk ->
      Printf.printf "    %s (in %s)\n" pk.package_name pk.package_dirname
    ) missed_by;
  ) pj.project_missing;
  Printf.printf "INCOMPLETE:\n%!";
  Array.iter (fun pk ->
    Printf.printf "  %s (in %s)\n" pk.package_name pk.package_dirname
  ) pj.project_incomplete;
  *)
  ()

let action () =
  let (_env_w, _p, _state, pj, _config_state) = BuildActionInit.init_env () in

  if !list_arg then
    do_list_packages pj
  else
    do_reply_to_queries pj;

  ()



let subcommand = {
  sub_name = "query";
  sub_help =  "Query information about environment.";
  sub_arg_list = arg_list;
  sub_arg_anon = None;
  sub_arg_usage = [ "Query information about environment."; ];
  sub_action = action;
}
