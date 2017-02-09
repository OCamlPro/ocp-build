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

open BuildEngineTypes

open BuildEngineContext
open BuildTypes
open BuildOCamlTypes

(*
let iter_requires f olib =
  List.iter (fun dep ->
    let opk = dep.dep_project in
    match opk.opk_lib with
    | None -> assert false
    | Some olib -> f olib dep
  ) olib.lib_opk.opk_requires
*)

let byte_exe =
  match MinUnix.os_type with
     MinUnix.WINDOWS
   | MinUnix.CYGWIN -> ".byte.exe"
   | MinUnix.UNIX -> ".byte"

let asm_exe =
  match MinUnix.os_type with
     MinUnix.WINDOWS
   | MinUnix.CYGWIN -> ".asm.exe"
   | MinUnix.UNIX -> ".asm"

let add_dst_file b dst_dir filename =
  add_file b dst_dir (Filename.basename filename)

exception NoSuchFileInDir of string * string

let find_dst_file dst_dir filename =
  try
    find_file dst_dir (Filename.basename filename)
  with Not_found ->
    raise (NoSuchFileInDir (filename, dst_dir.dir_fullname))
