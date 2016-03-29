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


let rec find_cmt_in_obuild_exn dirname basename =
  let maybe_filename =
    Filename.concat
      (Filename.concat dirname "_obuild/_mutable_tree") basename
  in
  if Sys.file_exists maybe_filename then
    let ic = open_in maybe_filename in
    let link = input_line ic in
    close_in ic;
    if Filename.is_relative link then
      Filename.concat dirname link
    else link
  else
    let new_dirname = Filename.dirname dirname in
    if new_dirname = dirname then raise Not_found else
      let dir_basename = Filename.basename dirname in
      find_cmt_in_obuild_exn new_dirname
        (Filename.concat dir_basename basename)

let find_cmt_in_obuild_exn filename =
  let filename =
    if Filename.is_implicit filename then
      Filename.concat (Sys.getcwd()) filename
    else filename in
  find_cmt_in_obuild_exn (Filename.dirname filename)
    (Filename.basename filename)

(* TODO: beware of is_relative used instead of is_implicit *)
