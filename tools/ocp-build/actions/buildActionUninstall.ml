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

(* ocp-build uninstall [OPTIONS]
*)

open Ezcmd.Modules
open OcpCompat

open BuildArgs

open BuildGlobals
open BuildUninstall.TYPES (* for package_uninstaller fields *)


(* TODO:
   * [ocp-build query PACKAGE] should use some of these arguments to set
   up the environment in which it is going to look for packages installed
   by ocp-build (ocp-build has information on tools, where ocamlfind has
   only META for libraries).
*)

let destdir = ref None
let scandirs = ref []
let format = ref "$name $version ($type) $dir"

type query = All | Program | Library

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

  set_action_arg "--list" ActionList
    " Only list installed packages";
  set_action_arg "--query" (ActionQuery All)
    " Query information on the following packages";
  set_action_arg "--query-program" (ActionQuery Program)
    " Query information on the following packages";
  set_action_arg "--query-library" (ActionQuery Library)
    " Query information on the following packages";

  "--install-lib", Arg.String (fun s ->
      scandirs := s :: !scandirs),
  "DIR Scan directory for packages to uninstall";
  "--format", Arg.String (fun s -> format := s),
  "FORMAT Format for query (with $name,$version,$dir,$type)";
  "--name", Arg.Unit (fun () -> format := "$name"),
  " Set format to '$name'";
  "--version", Arg.Unit (fun () -> format := "$version"),
  " Set format to '$version'";
  "--dir", Arg.Unit (fun () -> format := "$dir"),
  " Set format to '$dir'";
  "--type", Arg.Unit (fun () -> format := "$type"),
  " Set format to '$type'";

  "--destdir", Arg.String (fun s ->
      destdir := Some s),
  "DIR Set root of installation dir";
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
        try
          let env_PATH = BuildConfig.get_PATH () in
          let ocamlc = BuildConfig.find_in_PATH "ocamlc" env_PATH in
          let prefix = Filename.dirname (Filename.dirname ocamlc) in
          scandirs := [Filename.concat prefix "lib"]
        with Not_found ->
          Printf.eprintf
            "Error: you MUST at least use `-install-lib DIR` once.\n%!";
          exit 2
  end;

  let targets = List.rev !targets_arg in

  let state = BuildUninstall.init !destdir !scandirs in

  match !action with
  | None ->
     if targets = [] then begin
         Printf.eprintf "Error: no target specified\n";
         Printf.eprintf
           "  If you want to remove all targets from this project, use:\n";
         Printf.eprintf "      ocp-build install --uninstall\n";
         exit 2;
       end else begin
         List.iter (BuildUninstall.uninstall state) targets;
         BuildUninstall.finish state;
       end
  | Some (_,ActionList) ->
    begin match targets with
    | name :: _ ->
      Printf.eprintf "Error: unexpected argument %s after -list\n%!" name;
      exit 2
    | [] ->
      let packages = BuildUninstall.list_installed state in
      Printf.printf "Found %d packages that can be uninstalled\n%!" (List.length packages);
      List.iter (fun un ->
        Printf.printf "* %s %s (%s) %s\n%!"
          un.un_name un.un_version un.un_type un.un_directory;
      ) packages
    end


  | Some (_arg_name, ActionQuery query) ->
    let b = Buffer.create 100 in
    let print_target un =
      if match query with
      | All -> true
      | Program -> un.un_type = "program"
      | Library -> un.un_type = "library" then begin
        Buffer.clear b;
        Buffer.add_substitute b (function
        | "name" -> un.un_name
        | "version" -> un.un_version
        | "type" -> un.un_type
        | "dir" -> un.un_directory
        | s -> "$" ^ s) !format;
        Printf.printf "%s\n%!" (Buffer.contents b)
      end
        in
      let packages = BuildUninstall.list_installed state in
      let packages =
        let map = ref StringMap.empty in
        List.iter (fun un ->
          map :=  StringMap.add un.un_name un !map
        ) packages;
        !map
      in
      if targets = [] then begin
        StringMap.iter (fun _ un -> print_target un) packages;
        exit 0
      end;
      let not_found = ref false in
      List.iter (fun target ->
        try
          let un = StringMap.find target packages in
          print_target un
        with Not_found ->
          Printf.eprintf "Package %S not found.\n%!" target
      ) targets;
      if !not_found then exit 2

let subcommand = {
  Arg.cmd_name = "uninstall";
  cmd_man = [`P "Uninstall the project."];
  cmd_args = Arg.translate arg_list
             @ Arg.translate_anon arg_anon;
  cmd_doc = "Uninstall the project.";
  cmd_action = action;
}
