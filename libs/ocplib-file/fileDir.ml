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

open OcpCompat

let mkdir dir perm = MinUnix.mkdir (FileGen.to_string dir) perm
let make dir = mkdir dir 0o755

let rec safe_mkdir ?(mode=0o755) dir =
  if FileGen.exists dir then begin
    if not (FileGen.is_directory dir) then
      Printf.kprintf failwith "FileGen.Dir.make_all: %s not a directory"
        (FileGen.to_string dir)
  end
  else
  if FileGen.is_link dir then
    Printf.kprintf failwith
      "FileGen.Dir.make_all: %s is an orphan symbolic link"
      (FileGen.to_string dir)
  else begin
    let predir = FileGen.dirname dir in
    if predir != dir then safe_mkdir ~mode predir;
    if not (FileGen.exists dir) then
      try
        mkdir dir mode
      with e ->
        failwith (Printf.sprintf "FileGen.Dir.make_all: mkdir [%s] raised %s"
                    (FileGen.to_string dir) (Printexc.to_string e))
  end

let make_all dir = safe_mkdir dir

let list filename = Array.to_list (Sys.readdir (FileGen.to_string filename))

  let list_files filename =
    Array.to_list (
      Array.map (fun file -> FileGen.add_basename filename file)
        (Sys.readdir (FileGen.to_string filename)))

  let iter f dirname =
    Array.iter f (Sys.readdir (FileGen.to_string dirname))

  let iter_files f dirname =
    List.iter f (list_files dirname)

  let remove dir = MinUnix.rmdir (FileGen.to_string dir)

let rec remove_all (dir : FileGen.t) =
  iter_files (fun filename ->
      if not (FileGen.is_link filename) && FileGen.is_directory filename then
        remove_all filename
      else
        FileGen.remove filename
    ) dir;
  remove dir
