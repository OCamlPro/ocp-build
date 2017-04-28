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

let root_dir = ref (Sys.getcwd ())
let list_ocp_files = ref false
let packages = ref []

let arg_list = [
  "-root", Arg.String (fun s -> root_dir := s), " <dir> : root of project to scan";
  "-list-ocp-files", Arg.Set list_ocp_files, " : list .ocp files";
]

let arg_usage = Printf.sprintf "%s [OPTIONS] [PACKAGES] : display information on this OCP project" (Filename.basename Sys.argv.(0))

let arg_anon s = packages := s :: !packages

let print_dependencies pj packages =
  let packages_by_name = Hashtbl.create 113 in
  Array.iter (fun pk ->
    Hashtbl.add packages_by_name pk.package_name pk
  ) pj.project_sorted;

  let work_queue = ref [] in
  let deps = ref IntSet.empty in
  let add_package pk =
    if not (IntSet.mem pk.package_id !deps) then begin
      deps := IntSet.add pk.package_id !deps;
      work_queue := pk :: !work_queue
    end
  in
  List.iter (fun name ->
    try
      let pk = Hashtbl.find packages_by_name name in
      add_package pk
    with Not_found ->
      Printf.fprintf stderr "Error: unknown package %S\n%!" name;
      exit 2
  ) packages;

  let rec iter () =
    match !work_queue with
      [] -> ()
    | pk :: tail ->
      work_queue := tail;
      List.iter (fun pk -> add_package pk) pk.package_requires_list;
      iter ()
  in
  iter ();

  Array.iter (fun pk ->
    if IntSet.mem pk.package_id !deps then begin
      Printf.printf "%3d\t%S" pk.package_id pk.package_name;
      List.iter (fun pk -> Printf.printf " (%d)"
        pk.package_id)
        pk.package_requires_list;
      Printf.printf "\n%!"
    end
  ) pj.project_sorted;
  ()


let _ =
  Arg.parse arg_list arg_anon arg_usage;
  let packages = List.rev !packages in

  let root_dir = FileGen.of_string !root_dir in
  let files = BuildOCP.scan_root root_dir in
  if !list_ocp_files then
    List.iter (fun file ->
      Printf.printf "%S\n%!" (FileGen.to_string file)
    ) files;
  let state = BuildOCP.init_packages () in
  let config = BuildOCP.empty_config () in
  let nerrors = BuildOCP.load_ocp_files config state files in
  let w = BuildWarnings.empty_set () in
  let pj = BuildOCP.verify_packages w state in
  if nerrors > 0 then exit 2;
  print_dependencies pj packages;
  ()
