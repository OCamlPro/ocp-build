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

type oc = string * OcpCompat.Buffer.t

let post_commit_hooks = ref []
let add_post_commit_hook (f : unit -> string list) =
  post_commit_hooks := f :: !post_commit_hooks

let files = ref []
let write_file ?(exe=false) filename content =
  files := (filename, exe, content) :: !files


let commit filename =
  let old_files = ref StringMap.empty in
    begin try
        FileString.iter_lines (fun line ->
            let file, md5 = OcpString.cut_at line ' ' in
            old_files := StringMap.add file md5 !old_files
          ) filename
      with Sys_error _ -> ()
    end;
    let files = List.map (fun (file, exe, content) ->
        (file, content, exe, Digest.to_hex (Digest.string content))
      ) !files in
    let modified = ref false in
    Printf.eprintf "Proposed list of changes:\n%!";
    List.iter (fun (file, _content, _exe, new_md5) ->
        try
          let old_md5 = StringMap.find file !old_files in
          let current_md5 = try
              Digest.to_hex (Digest.file file)
            with _ -> (* file probably removed by user action *)
              raise Not_found
          in
          if current_md5 <> old_md5 then begin
            Printf.eprintf "x file %S: file has been modified since last use.\n%!" file;
            modified := true;
          end;
          if current_md5 <> new_md5 then begin
            Printf.eprintf "* file %S: content will be updated\n%!" file;
          end else begin
            Printf.eprintf "  File %S: no change\n%!" file;
          end
        with Not_found ->
          Printf.eprintf "+ File %S: a new generated file\n%!" file;
      ) files;
    if !modified && not !AutoconfArgs.arg_force then begin
      Printf.eprintf "Error: some files have been modified since last use.\n%!";
      Printf.eprintf "Update 'manage_files' or use '-f' to force overwrite\n%!";
      Printf.eprintf "Aborting: no modification done.\n%!";
      exit 2
    end;
    List.iter (fun (file, content, exe, digest) ->
        let dirname = Filename.dirname file in
        FileString.safe_mkdir dirname;
        FileString.write_file file content;
        if file = "configure" || exe then begin
          Unix.chmod file 0o755;
        end;
        old_files := StringMap.add file digest !old_files
      ) files;

    let oc = open_out filename in
    StringMap.iter (fun file md5 ->
        Printf.fprintf oc "%s %s\n" file md5;
      ) !old_files;
    close_out oc;

    let files = filename :: List.map (fun (file,_, _,_) -> file) files in
    let files = ref files in

    List.iter (fun hook ->
        files := hook () @ !files) !post_commit_hooks;

    !files


let open_out filename =
  (filename, Buffer.create 1000)
let fprintf (_,b) = Printf.bprintf b
let output_string (_,b) = Buffer.add_string b
let close_out (filename, b) =
  write_file filename (Buffer.contents b)
