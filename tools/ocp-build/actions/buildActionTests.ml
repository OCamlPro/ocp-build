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

open BuildArgs
open BuildActions
open BuildTypes

let arg_list = []




let do_test b ncores projects =
  time_step "Executing tests";
  let stats = BuildOCamlTest.init () in
  List.iter (fun p ->
     let module P = (val p : Package) in
    let lib = P.info in
   match lib.lib_type with
    | ProgramPackage
    | TestPackage ->
      BuildOCamlTest.test_package b stats lib !benchmarks_arg
    | LibraryPackage
    | ObjectsPackage
    | SyntaxPackage
    | RulesPackage
      -> ()
  ) projects;
  BuildOCamlTest.finish stats ncores;
  time_step "   Done executing tests"
;;

let action () =
  BuildActionMake.(
    if not !make_doc_targets && not !make_test_targets then make_build_targets := true;
    (* Test targets require build targets ? *)
    if !make_test_targets then make_build_targets := true;
    if !make_doc_targets then make_build_targets := true;
  );


  let (p, bc, projects, _package_map) = BuildActionMake.do_build () in
  do_test bc.build_context (BuildActionMake.get_ncores p.cin) projects;
  ()

let subcommand = {
  Arg.cmd_name = "tests";
  cmd_man = [`P "Run project tests."];
  cmd_args = arg_list
             @ BuildActionMake.arg_list
             @ Arg.translate_anon arg_anon;
  cmd_doc = "Run project tests.";
  cmd_action = action;
}
