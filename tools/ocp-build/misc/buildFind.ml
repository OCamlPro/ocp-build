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
