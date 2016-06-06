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
open SimpleConfig.Op (* !! and =:= *)
open AutoconfArgs

let apply_makers () =
  List.iter (fun file ->
      try
        let maker = StringMap.find file !AutoconfCommon.makers in
        Printf.eprintf "Calling maker for %S\n%!" file;
        maker ();
        AutoconfCommon.makers := StringMap.remove file !AutoconfCommon.makers;
      with Not_found ->
        Printf.eprintf "Warning: no maker found for file %S\n%!" file
    ) !!AutoconfProjectConfig.manage_files;
  Printf.eprintf "Unactive makers:  ";
  StringMap.iter (fun file _ ->
      Printf.eprintf "%s  " file;
    ) !AutoconfCommon.makers;
  Printf.eprintf "\n%!";
  ()

let () =
  Arg.parse AutoconfArgs.arg_list AutoconfArgs.arg_anon AutoconfArgs.arg_usage;

  AutoconfGlobalConfig.load ();
  AutoconfProjectConfig.load ();

  if !arg_git_add then begin
    if not (Sys.file_exists ".git") then
      AutoconfCommon.command "git init"
  end;

  apply_makers ();

  let files = AutoconfFS.commit "ocp-autoconf.files" in

  if !arg_git_add then begin
    let cmd = Printf.sprintf "git add %s" (String.concat " " files) in
    AutoconfCommon.command cmd
  end;
  ()
