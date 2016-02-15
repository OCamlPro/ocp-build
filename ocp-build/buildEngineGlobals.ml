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



let file_filename file = File.to_string file.file_file
(*  Printf.eprintf "File dir = %d\n" file.file_dir.dir_id; *)
(*  Filename.concat file.file_dir.dir_fullname file.file_basename *)


