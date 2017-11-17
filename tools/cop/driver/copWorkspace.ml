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

(*
   A workspace for COP is where all the projects to build are. It is
  usually specified by a file `cop-workspace` at the root of the workspace
  (changed using `--workspace WORKSPACE`).
  By default, COP will set its root at the largest surrounding workspace, unless
  `--local` is specified or `--root DIR` is specified).

  COP will then look for description files in the workspace directories:
  * `cop-project` files contain information on targets to build
  * `FILE.cop` files contain generic code (module files)
  * `cop-ignore` can be used to ignore specific sub-directories

  Modules files are interpreted once at the beginning. Then, COP will
  execute the workspace file (`cop-workspace`) and when asked for
  will execute all the `cop-project` files for each switch.
 *)

open OcpCompat (* StringSet *)

exception NoWorkspace of string

let rec lookup_workspace dir candidate ~local ~workspace =
  let candidate =
    if Sys.file_exists (Filename.concat dir workspace) then
      Some dir
    else
      candidate
  in
  if local && candidate != None then
    candidate
  else
    let dirname = Filename.dirname dir in
    if dirname = dir then
      candidate
    else
      lookup_workspace dirname candidate ~local ~workspace

let lookup_root ~root ~local ~workspace =
  let root =
    match root with
    | Some root ->
       let filename = Filename.concat root workspace in
       if not (Sys.file_exists filename) then
         raise (NoWorkspace filename);
       root
    | None ->
       match
         lookup_workspace  (Sys.getcwd ()) None ~local ~workspace
       with
       | None -> raise (NoWorkspace workspace)
       | Some root -> root
  in
  Printf.printf "Entering directory '%s'\n%!" root;
  Unix.chdir root;
  root

let scan_workspace root =
  let ignored = ref StringSet.empty in

  let project_files = ref [] in
  let module_files = ref [] in

  let f _dirname basename fullname =
    if StringSet.mem fullname !ignored ||
         match basename.[0] with
         | '_' | '.' -> true
         | _ -> false
    then
      BuildScanner.ignore_file_or_directory ();

    match basename with
    | "cop-project" ->
       project_files := fullname :: !project_files
    | "cop-ignore" ->
       let files = FileLines.read_file fullname in
       List.iter (fun file ->
           ignored := StringSet.add file !ignored) files
    | _ ->
       if Filename.check_suffix basename ".cop" then
         module_files := fullname :: !module_files
  in
  BuildScanner.scan_directory f root;
  List.rev !project_files,
  !module_files
