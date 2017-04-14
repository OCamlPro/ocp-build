(**************************************************************************)
(*                                                                        *)
(*   Typerex Libraries                                                    *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open StringCompat

let mkdir dir perm = MinUnix.mkdir (FileAbs.to_string dir) perm
let make dir = mkdir dir 0o755

let rec safe_mkdir ?(mode=0o755) dir =
  if FileAbs.exists dir then begin
    if not (FileAbs.is_directory dir) then
      Printf.kprintf failwith "FileAbs.Dir.make_all: %s not a directory"
        (FileAbs.to_string dir)
  end
  else
  if FileAbs.is_link dir then
    Printf.kprintf failwith
      "FileAbs.Dir.make_all: %s is an orphan symbolic link"
      (FileAbs.to_string dir)
  else begin
    let predir = FileAbs.dirname dir in
    if predir != dir then safe_mkdir ~mode predir;
    if not (FileAbs.exists dir) then
      try
        mkdir dir mode
      with e ->
        failwith (Printf.sprintf "FileAbs.Dir.make_all: mkdir [%s] raised %s"
                    (FileAbs.to_string dir) (Printexc.to_string e))
  end

let make_all dir = safe_mkdir dir

let list filename = Array.to_list (Sys.readdir (FileAbs.to_string filename))

  let list_files filename =
    Array.to_list (
      Array.map (fun file -> FileAbs.add_basename filename file)
        (Sys.readdir (FileAbs.to_string filename)))

  let iter f dirname =
    Array.iter f (Sys.readdir (FileAbs.to_string dirname))

  let iter_files f dirname =
    List.iter f (list_files dirname)

  let remove dir = MinUnix.rmdir (FileAbs.to_string dir)

let rec remove_all (dir : FileAbs.t) =
  iter_files (fun filename ->
      if not (FileAbs.is_link filename) && FileAbs.is_directory filename then
        remove_all filename
      else
        FileAbs.remove filename
    ) dir;
  remove dir
