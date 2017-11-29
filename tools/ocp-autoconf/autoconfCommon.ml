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

let homedir = try
    Sys.getenv "HOME"
  with Not_found -> "/"

let curdir = Sys.getcwd ()

let find_content filename =
  try
    List.assoc filename AutoconfFiles.files
  with Not_found ->
    Printf.eprintf "Template for file %S not found\n%!" filename;
    exit 2

let save_file ?exe ?(override=true) filename =
  assert (OcpString.starts_with filename ~prefix:"skeleton/");
  let _,dst_filename = OcpString.cut_at filename '/' in
  if override || not (Sys.file_exists dst_filename) then
    let content = find_content filename in
    AutoconfFS.write_file ?exe dst_filename content;
    ()

let command cmd =
  Printf.eprintf "Calling %s...\n%!" cmd;
  let code = Sys.command cmd in
  if code <> 0 then begin
    Printf.eprintf "Error: %S returned non-zero status (%d)\n%!" cmd code;
    exit 2
  end

let makers = ref StringMap.empty

let register_maker file (maker : unit -> unit) =
  if StringMap.mem file !makers then begin
    Printf.eprintf "Error: two makers for files %S\n%!" file;
    exit 2
  end;
  makers := StringMap.add file maker !makers
