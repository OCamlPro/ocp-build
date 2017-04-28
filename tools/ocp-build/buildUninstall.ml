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

module TYPES = struct

  type raw_kind = DIR | FILE | VERSION | WARNING | TYPE | PACK

type package_uninstaller = {
  mutable un_nfiles : int;
  mutable un_ndirs : int;
  mutable un_version : string;
  mutable un_name : string;
  mutable un_descr : string;
  mutable un_warning : string option;
  mutable un_directory : string;
  mutable un_type : string;
  mutable un_packages : string list;
}

end

open TYPES

type state = {
  un_destdir : string option;
  un_scandirs : string list;
  mutable un_done : StringSet.t;
  mutable un_dirs : string list;
  mutable un_errors : int;
}

let in_destdir where file =
  match where.un_destdir with
    None -> file
  | Some destdir -> Filename.concat destdir file

let uninstallers = Hashtbl.create 13

let rec scan_for_uninstallers where files libdirs =
  match libdirs with
    [] -> files
  | libdir :: libdirs ->
    let files = ref files in
    let libdir_d = in_destdir where libdir in
    BuildScanner.scan_directory_for_suffix libdir_d ".uninstall"
      (fun filename ->
(*        Printf.eprintf "UINSTALLER %S\n%!" filename; *)
        let name = Filename.chop_suffix (Filename.basename filename)
          ".uninstall" in
        files := StringMap.add name filename !files);
    scan_for_uninstallers where !files libdirs

let load_uninstallers where =
  try
    Hashtbl.find uninstallers where.un_scandirs
  with Not_found ->
    let files = scan_for_uninstallers where StringMap.empty
      where.un_scandirs in
    Hashtbl.add uninstallers where.un_scandirs files;
    files

let find_uninstaller where lib_name =
  let uninstallers = load_uninstallers where in
  try
    Some (StringMap.find lib_name uninstallers)
  with Not_found -> None

let init un_destdir un_scandirs =
  {
    un_destdir; un_scandirs;
    un_done = StringSet.empty;
    un_dirs = [];
    un_errors = 0;
  }

let rec uninstall_by_uninstaller state uninstall_file_d =
  let lib_name = Filename.chop_suffix (Filename.basename uninstall_file_d)
    ".uninstall"
  in
  if not (Sys.file_exists uninstall_file_d) then begin
    Printf.eprintf "Warning: uninstaller file %S not found\n%!"
      uninstall_file_d;
    state.un_errors <- state.un_errors + 1
  end else
    let list = FileLines.read_file uninstall_file_d in
    List.iter (fun line ->
      match OcpString.cut_at line ' ' with
      | "OCP", _ -> ()
      | "REG", file ->
        let file_d = in_destdir state file in
        if Sys.file_exists file_d then begin try
          Sys.remove file_d
        with e ->
          Printf.eprintf
            "Warning: exception %S while removing regular file %S\n%!"
            (Printexc.to_string e) file_d
        end
      | "DIR", file ->
        let file_d = in_destdir state file in
        state.un_dirs <- file_d :: state.un_dirs
      | "VER", _version -> ()
      | "WAR", _warning -> ()
      | "LOG", _log -> ()
      | "TYP", _kind -> ()
      | "PCK", name ->
        let name =
          if Filename.check_suffix name ".uninstall" then
            Filename.chop_suffix (Filename.basename name) ".uninstall"
          else name
        in
        schedule_uninstall state name
      | _ ->
        Printf.eprintf "Bad line [%S] in file %S\n%!" line uninstall_file_d;
    ) list;
    Printf.printf "Package %s uninstalled\n%!" lib_name

and schedule_uninstall state lib_name =
  if not (StringSet.mem lib_name state.un_done) then begin
    state.un_done <- StringSet.add lib_name state.un_done;
      match find_uninstaller state lib_name with
        None ->
        Printf.eprintf
          "Warning, uninstall of %S failed:\n" lib_name;
        Printf.eprintf "   could not find uninstaller file %S\n%!"
          (lib_name ^ ".uninstall");
        state.un_errors <- state.un_errors + 1
    | Some uninstall_file ->
      uninstall_by_uninstaller state uninstall_file
  end

let uninstall state lib_name =
  schedule_uninstall state lib_name

let finish state =
  let dirs = state.un_dirs in
  let dirs = List.sort compare dirs in
  let dirs = List.rev dirs in
  List.iter (fun file_d ->
    if Sys.file_exists file_d then begin try
      MinUnix.rmdir file_d
    with e ->
      Printf.eprintf
        "Warning: exception %S while removing directory %S\n%!"
        (Printexc.to_string e) file_d
    end
  ) dirs

let load_uninstaller filename =
    let list = FileLines.read_file filename in
    let name = Filename.chop_suffix (Filename.basename filename) ".uninstall" in
    let un = {
      un_nfiles = 0;
      un_ndirs = 0;
      un_version = "n/a";
      un_name = name;
      un_descr = name;
      un_warning = None;
      un_directory = Filename.dirname filename;
      un_type = "n/a";
      un_packages = [];
    } in
    List.iter (fun line ->
      match OcpString.cut_at line ' ' with
      | "OCP", _ -> ()
      | "REG", _file -> un.un_nfiles <- 1 + un.un_nfiles
      | "DIR", _file -> un.un_ndirs <- 1 + un.un_ndirs
      | "VER", v -> un.un_version <- v
      | "WAR", w -> un.un_warning <- Some w
      | "LOG", d -> un.un_descr <- d
      | "TYP", t -> un.un_type <- t
      | "PCK", pck -> un.un_packages <- pck :: un.un_packages
      | _ ->
        Printf.eprintf "Bad line [%S] in file %S\n%!" line filename;
    ) list;
    un

let list_installed where =
  let uninstallers = load_uninstallers where in
  let list = ref [] in
  StringMap.iter (fun _ filename ->
    list := load_uninstaller filename :: !list) uninstallers;
  !list

let is_installed where lib_name =
  (find_uninstaller where lib_name) <> None

(*
open BuildTypes

let uninstall_package state lib =
  if lib.lib_install then uninstall state lib.lib_name

(* val uninstall_package : state -> BuildTypes.package_info -> unit *)
*)





type raw_uninstaller = (raw_kind * string) list ref

let create_un () = ref []
let add_un_field log kind name =
  log := (kind, name) :: !log

let save_un uninstall_file log =
  let oc = open_out uninstall_file in
  Printf.fprintf oc "OCP 1\n";
  List.iter (fun (kind, file) ->
    Printf.fprintf oc "%s %s\n" (match kind with
      FILE -> "REG"
    | DIR -> "DIR"
    | VERSION -> "VER"
    | WARNING -> "WAR"
    | TYPE -> "TYP"
    | PACK -> "PCK"
    ) (String.escaped file);
  ) !log;
  close_out oc
