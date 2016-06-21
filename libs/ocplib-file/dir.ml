(**************************************************************************)
(*                                                                        *)
(*                              OCamlPro TypeRex                          *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the LGPL v2.1 with       *)
(*   the special exception on linking described in the file LICENSE.      *)
(*      (GNU Lesser General Public Licence version 2.1)                   *)
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

let mkdir dir perm = MinUnix.mkdir (File.to_string dir) perm
let make dir = mkdir dir 0o755

let rec make_all dir =
  if File.exists dir then begin
    if not (File.is_directory dir) then
      Printf.kprintf failwith "File.Dir.make_all: %s not a directory"
        (File.to_string dir)
  end
  else
  if File.is_link dir then
    Printf.kprintf failwith
      "File.Dir.make_all: %s is an orphan symbolic link"
      (File.to_string dir)
  else begin
    let predir = File.dirname dir in
    if predir != dir then make_all predir;
    if not (File.exists dir) then
      try
        mkdir dir 0o775
      with e ->
        failwith (Printf.sprintf "File.Dir.make_all: mkdir [%s] raised %s"
                    (File.to_string dir) (Printexc.to_string e))
  end

let list filename = Array.to_list (Sys.readdir (File.to_string filename))

  let list_files filename =
    Array.to_list (
      Array.map (fun file -> File.add_basename filename file)
        (Sys.readdir (File.to_string filename)))

  let iter f dirname =
    Array.iter f (Sys.readdir (File.to_string dirname))

  let iter_files f dirname =
    List.iter f (list_files dirname)

  let remove dir = MinUnix.rmdir (File.to_string dir)

let rec remove_all (dir : File.t) =
  iter_files (fun filename ->
      if not (File.is_link filename) && File.is_directory filename then
        remove_all filename
      else
        File.remove filename
    ) dir;
  remove dir
