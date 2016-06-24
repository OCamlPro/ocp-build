(**************************************************************************)
(*                                                                        *)
(*                              OCamlPro TypeRex                          *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*      (GNU Public Licence version 3.0).                                 *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)

open BuildArgs
open BuildActions
open BuildTypes

let arg_list =
  BuildOptions.merge
    [
      [

      ];
      BuildActionBuild.arg_list
    ]



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
  BuildActionBuild.(
    if not !make_doc_targets && not !make_test_targets then make_build_targets := true;
    (* Test targets require build targets ? *)
    if !make_test_targets then make_build_targets := true;
    if !make_doc_targets then make_build_targets := true;
  );


  let (p, bc, projects, _package_map) = BuildActionBuild.do_build () in
  do_test bc.build_context (BuildActionBuild.get_ncores p.cin) projects;
  ()

let subcommand = {
  sub_name = "tests";
  sub_help =  "Run project tests.";
  sub_arg_list = arg_list;
  sub_arg_anon = Some arg_anon;
  sub_arg_usage = [ "Run project tests."; ];
  sub_action = action;
}
