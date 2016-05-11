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


(* ocp-build uninstall [OPTIONS]
*)

open StringCompat

open BuildArgs

open BuildGlobals
open BuildUninstall (* for package_uninstaller fields *)


(* TODO:
   * [ocp-build query PACKAGE] should use some of these arguments to set
   up the environment in which it is going to look for packages installed
   by ocp-build (ocp-build has information on tools, where ocamlfind has
   only META for libraries).
*)

let destdir = ref None
let scandirs = ref []

type query = All | Directory | Version

type action =
  | ActionList
  | ActionQuery of query


let action = ref None
let set_action_arg arg_name arg_action arg_help =
  let arg_action () =
    match !action with
    | None ->
      action := Some (arg_name, arg_action)
    | Some (old_arg, old_action) ->
      if old_action <> arg_action then begin
        Printf.eprintf "Error: actions %s and %s specified on the same command.\n%!" old_arg arg_name;
        exit 2
      end
  in
  (arg_name, Arg.Unit arg_action, arg_help)

let arg_list = [
  "-destdir", Arg.String (fun s ->
      destdir := Some s),
  "DIR Set root of installation dir";

  "-install-lib", Arg.String (fun s ->
      scandirs := s :: !scandirs),
  "DIR Scan directory for packages to uninstall";

  set_action_arg "-list" ActionList
    " Only list installed packages";
  set_action_arg "-query" (ActionQuery All)
    " Query information on the following packages";
  set_action_arg "-query-dir" (ActionQuery Directory)
    " Query directory information on the following packages";
  set_action_arg "-query-version" (ActionQuery Version)
    " Query information on the following packages";
]

let action () =

  if !scandirs = [] then begin
    try
      let opam_prefix = Sys.getenv "OPAM_PREFIX" in
      scandirs := [Filename.concat opam_prefix "lib"]
    with Not_found ->
      try
        let ocamllib = Sys.getenv "OCAMLLIB" in
        scandirs := [ocamllib]
      with Not_found ->
        Printf.eprintf
          "Error: you MUST at least use `-install-lib DIR` once.\n%!";
        exit 2
  end;

  let targets = List.rev !targets_arg in

  let state = BuildUninstall.init !destdir !scandirs in

  match !action with
  | None ->
    List.iter (BuildUninstall.uninstall state) targets;
    BuildUninstall.finish state;
  | Some (_,ActionList) ->
    begin match targets with
      | name :: _ ->
        Printf.eprintf "Error: unexpected argument %s after -list\n%!" name;
        exit 2
      | [] ->
        let packages = BuildUninstall.list_installed state in
        Printf.printf "Found %d packages that can be uninstalled\n%!" (List.length packages);
        List.iter (fun un ->
            Printf.printf "* %s version %s type %s dir %s\n%!" un.un_name un.un_version un.un_type un.un_directory;
          ) packages
    end


  | Some (arg_name, ActionQuery query) ->
    if targets = [] then begin
      Printf.eprintf "Warning: %s with no package specified\n%!" arg_name;
      exit 0
    end;
    let packages = BuildUninstall.list_installed state in
    let packages =
      let map = ref StringMap.empty in
      List.iter (fun un ->
          map :=  StringMap.add un.un_name un !map
        ) packages;
      !map
    in
    let not_found = ref false in
    List.iter (fun target ->
        try
          let un = StringMap.find target packages in
          match query with
          | All ->
            Printf.printf "* %s version %s type %s dir %s\n%!" un.un_name un.un_version un.un_type un.un_directory
          | Directory ->
            Printf.printf "%s\n%!" un.un_directory
          | Version ->
            Printf.printf "%s\n%!" un.un_version
        with Not_found ->
          Printf.eprintf "Package %S not found.\n%!" target
      ) targets;
    if !not_found then exit 2

let subcommand = {
  sub_name = "uninstall";
  sub_help =  "Uninstall the project.";
  sub_arg_list = arg_list;
  sub_arg_anon = Some arg_anon;
  sub_arg_usage = [ "Uninstall the project."; ];
  sub_action = action;
}
