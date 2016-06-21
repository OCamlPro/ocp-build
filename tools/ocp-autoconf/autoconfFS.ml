(**************************************************************************)
(*                                                                        *)
(*                              OCamlPro TypeRex                          *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*      (GNU Public Licence version 3.0).                                 *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)

open StringCompat

let post_commit_hooks = ref []
let add_post_commit_hook (f : unit -> string list) =
  post_commit_hooks := f :: !post_commit_hooks

let files = ref []
let write_file filename content =
  files := (filename, content) :: !files

let create_file filename =
  (filename, Buffer.create 1000)
let fprintf (_,b) = Printf.bprintf b
let output_string (_,b) = Buffer.add_string b
let close_file (filename, b) =
  write_file filename (Buffer.contents b)

let commit filename =
  let old_files = ref StringMap.empty in
    begin try
        FileString.iter_lines (fun line ->
            let file, md5 = OcpString.cut_at line ' ' in
            old_files := StringMap.add file md5 !old_files
          ) filename
      with Sys_error _ -> ()
    end;
    let files = List.map (fun (file, content) ->
        (file, content, Digest.to_hex (Digest.string content))
      ) !files in
    let modified = ref false in
    Printf.eprintf "Proposed list of changes:\n%!";
    List.iter (fun (file, _content, new_md5) ->
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
    List.iter (fun (file, content, digest) ->
        let dirname = Filename.dirname file in
        FileString.safe_mkdir dirname;
        FileString.write_file file content;
        if file = "configure" then begin
          Unix.chmod "configure"  0o755;
        end;
        old_files := StringMap.add file digest !old_files
      ) files;

    let oc = open_out filename in
    StringMap.iter (fun file md5 ->
        Printf.fprintf oc "%s %s\n" file md5;
      ) !old_files;
    close_out oc;

    let files = filename :: List.map (fun (file,_,_) -> file) files in
    let files = ref files in

    List.iter (fun hook ->
        files := hook () @ !files) !post_commit_hooks;

    !files
