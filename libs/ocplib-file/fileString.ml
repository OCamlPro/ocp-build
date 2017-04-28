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

(* type t = string *)

let cut_at_last_extension basename =
  try
    let pos = String.rindex basename '.' in
    OcpString.before basename pos,
    String.lowercase (OcpString.after basename pos)
  with Not_found -> (basename, "")

let extensions_of_basename basename =
  match OcpString.split basename '.' with
    [] | [_] -> []
  | _basename :: exts -> exts

let last_extension extensions =
  try
    Some (OcpList.last extensions)
  with Not_found -> None

let extensions file = extensions_of_basename (Filename.basename file)

let is_absolute file = not (Filename.is_relative file)
let is_relative = Filename.is_relative
let is_implicit = Filename.is_implicit
let concat = Filename.concat
let add_basename = Filename.concat
let add_basenames = List.fold_left Filename.concat
let dirname = Filename.dirname
let basename = Filename.basename
let check_suffix = Filename.check_suffix
let add_suffix = (^)
let chop_extension = Filename.chop_extension



let open_in = open_in
let open_in_bin = open_in_bin
let open_out = open_out
let open_out_bin = open_out_bin

let open_fd file mode perm = MinUnix.openfile file mode perm

let temp_file prefix suffix = Filename.temp_file prefix suffix
let current_dir_name = Filename.current_dir_name



let copy_file f1 f2 =
  let ic = open_in_bin f1 in
  let oc = open_out_bin f2 in
  FileChannel.copy_file ic oc;
  close_in ic;
  close_out oc

let iter_blocks f file =
  let ic = open_in_bin file in
  FileChannel.iter_blocks f ic;
  close_in ic

let iter_dir f dirname =
  let files = Sys.readdir dirname in
  Array.iter f files

let iter_files ?(recursive=false) f dirname =
  let rec iter dirname dir =
    let files = Sys.readdir (Filename.concat dirname dir) in
    Array.iter (fun file ->
        let file = Filename.concat dir file in
        if Sys.is_directory (Filename.concat dirname file) then begin
          if recursive then iter dirname file
        end else
          f file
      ) files
  in
  iter dirname ""

let rec safe_mkdir ?(mode=0o755) filename =
  try
    let st = MinUnix.stat filename in
    match st.MinUnix.st_kind with
      MinUnix.S_DIR -> ()
    | _ ->
      failwith (Printf.sprintf
                  "File.safe_mkdir: %S exists, but is not a directory"
                  filename)
  with MinUnix.Unix_error (MinUnix.ENOENT, _, _) ->
    let dirname = Filename.dirname filename in
    safe_mkdir ~mode dirname;
    let basename = Filename.basename filename in
    match basename with
    | "." | ".." -> ()
    | _ ->
      MinUnix.mkdir filename mode

(* [dst] must be the target file name, not the name of its
   directory *)
let rec copy_rec src dst =
  (*    Printf.eprintf "copy_rec: %S -> %S\n%!" src dst; *)
  match (MinUnix.stat src).MinUnix.st_kind with
  | MinUnix.S_DIR ->
    safe_mkdir dst;
    iter_dir (fun basename ->
        copy_rec (Filename.concat src basename)
          (Filename.concat dst basename)) src
  | MinUnix.S_REG ->
    copy_file src dst
  | _ ->
    failwith (Printf.sprintf
                "File.copy_rec: cannot copy unknown kind file %S"
                src)

  (* [dst] must be the target file name, not the name of its directory *)
let rec uncopy_rec src dst =
  match
    (try Some (MinUnix.stat src).MinUnix.st_kind with _ -> None),
    (try Some (MinUnix.stat dst).MinUnix.st_kind with _ -> None)
  with
  | _, None -> ()
  | Some MinUnix.S_DIR, Some MinUnix.S_DIR ->
    iter_dir (fun basename ->
        uncopy_rec (Filename.concat src basename)
          (Filename.concat dst basename)) src;
    (try MinUnix.rmdir dst with _ -> ())
  | Some MinUnix.S_REG, Some MinUnix.S_REG ->
    Sys.remove dst
  | _ ->
    failwith (Printf.sprintf
                "File.uncopy_rec: inconsistent kinds between %S and %S"
                src dst)



let write_file filename str =
  let oc = open_out_bin filename in
  output_string oc str;
  close_out oc
let file_of_string = write_file

let read_file filename =
  let ic = open_in_bin filename in
  try
    let s = FileChannel.read_file ic in
    close_in ic;
    s
  with e ->
    close_in ic;
    raise e
let string_of_file = read_file


let read_lines file =
  let ic = open_in file in
  let lines = FileChannel.read_lines ic in
  close_in ic;
  lines

let read_lines_to_revlist file =
  let ic = open_in file in
  let lines = FileChannel.read_lines_to_revlist ic in
  close_in ic;
  lines

let write_lines filename lines =
  let oc = open_out filename in
  Array.iter (fun l -> FileChannel.output_line oc l) lines;
  close_out oc

let lines_of_file = read_lines
let file_of_lines = write_lines

let iter_lines f name =
  let ic = open_in name in
  try
    FileChannel.iter_lines f ic;
    close_in ic
  with
  | e -> close_in ic; raise e

let iteri_lines f name =
  let ic = open_in name in
  try
    FileChannel.iteri_lines f ic;
    close_in ic
  with
  | e -> close_in ic; raise e

let read_sublines file off len =
  let ic = open_in file in
  try
    let lines = FileChannel.read_sublines ic off len in
    close_in ic;
    lines
  with exn ->
    close_in ic;
    raise exn

let read_subfile filename pos len =
  let ic = open_in_bin filename in
  try
    let b = FileChannel.read_subfile ic pos len in
    close_in ic;
    b
  with exn ->
    close_in ic;
    raise exn

let string_of_subfile = read_subfile







let is_directory = Sys.is_directory

let is_link filename =
  try let s = MinUnix.lstat filename in
    s.MinUnix.st_kind = MinUnix.S_LNK with _ -> false

let rename = Sys.rename
let remove = Sys.remove
let getcwd = Sys.getcwd
let exists = Sys.file_exists
let stat filename = MinUnix.stat filename
let lstat filename = MinUnix.lstat filename


let size filename =
  let s = MinUnix.stat filename in
  s.MinUnix.st_size

(*
let size64 filename =
  let s = MinUnix.LargeFile.stat filename in
  s.MinUnix.LargeFile.st_size
*)

module Op = struct

  let (//) = Filename.concat

  end
