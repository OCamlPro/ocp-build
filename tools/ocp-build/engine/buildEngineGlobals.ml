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

(* open BuildRules *)


(* open BuildBase *)
open BuildEngineTypes

let new_dir_id b =
  b.build_next_dir_id <- b.build_next_dir_id + 1;
  b.build_next_dir_id

let new_file_id b =
  b.build_next_file_id <- b.build_next_file_id + 1;
  b.build_next_file_id

let new_rule_id b =
  b.build_next_rule_id <- b.build_next_rule_id + 1;
  b.build_next_rule_id


let new_process_id b =
  b.build_next_process_id <- b.build_next_process_id + 1;
  b.build_next_process_id



let file_filename file = FileGen.to_string file.file_file
(*  Printf.eprintf "File dir = %d\n" file.file_dir.dir_id; *)
(*  Filename.concat file.file_dir.dir_fullname file.file_basename *)
