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

let save_file ?(override=true) filename =
  assert (OcpString.starts_with filename "skeleton/");
  let _,dst_filename = OcpString.cut_at filename '/' in
  if override || not (Sys.file_exists dst_filename) then
    let content = find_content filename in
    AutoconfFS.write_file dst_filename content;
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
