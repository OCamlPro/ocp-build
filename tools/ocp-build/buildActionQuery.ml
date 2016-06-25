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


(* ocp-build install [OPTIONS]

  Set the options of the user preference file.

*)









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
    List.iter (fun dep ->
      Printf.printf "%s %s "
        dep.dep_project.package_name
        (match dep.dep_link, dep.dep_syntax with
         | true, false -> ""
         | false, false -> "(none)"
         | false, true -> "(syntax)"
         | true, true -> "(link+syntax)")
    ) pk.pi.package_requires;
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
  let (_env_w, p, state, pj) = BuildActionInit.init_env () in

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
