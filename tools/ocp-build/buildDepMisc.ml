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
open BuildEngineTypes
open BuildTypes
open BuildOCPTypes
open BuildValue.TYPES

let verbose = OcpDebug.verbose_function [ "B" ;"BuildOCamldep" ]

let parse_dependencies b =
  let s = Buffer.contents b in
  let len = String.length s in
  let dependencies = ref [] in

  let rec parse_name pos pos0 =
(*    Printf.eprintf "parse_name %d %d\n%!" pos pos0; *)
    if pos+1 < len && s.[pos] = ':' && s.[pos+1] = ' ' then
      let target = String.sub s pos0 (pos-pos0) in
      skip_spaces target [] (pos+1)
    else
      if pos+1 < len && s.[pos] = ':' && s.[pos+1] = '\n' then begin
   let target = String.sub s pos0 (pos-pos0) in
   dependencies := (target, []) :: !dependencies;
   parse_name (pos+2) (pos+2)
      end else
   if pos = len then
     !dependencies
   else
     parse_name (pos+1) pos0

  and skip_spaces target deps pos =
(*    Printf.eprintf "skip_spaces %d\n%!" pos; *)
    if pos = len then
      (target, deps) :: !dependencies
    else
      match s.[pos] with
     '\\' when pos+1 < len && s.[pos+1] = '\n' ->
       skip_spaces target deps (pos+2)
   | ' ' ->
     skip_spaces target deps (pos+1)
   | '\n' ->
     dependencies := (target, deps) :: !dependencies;
     parse_name (pos+1) (pos+1)
   | '\\' when pos + 1 < len && s.[pos+1] = ' ' ->
     Buffer.clear b;
     Buffer.add_char b ' ';
     parse_dependency target deps (pos+2)
   | c ->
     Buffer.clear b;
     Buffer.add_char b c;
     parse_dependency target deps (pos+1)

  and parse_dependency target deps pos =
(*    Printf.eprintf "parse_dependency %d\n%!" pos; *)
    if pos = len then
      (target, (Buffer.contents b) :: deps) :: !dependencies
    else
      match s.[pos] with
   | '\\' when pos + 1 < len && s.[pos+1] = ' ' ->
     Buffer.add_char b ' ';
     parse_dependency target deps (pos+2)
   | ' ' ->
     skip_spaces target ((Buffer.contents b) :: deps) (pos+1)
   | '\n' ->
     dependencies := (target,
     (Buffer.contents b) :: deps) :: !dependencies;
     parse_name (pos+1) (pos+1)
   | c ->
     Buffer.add_char b c;
     parse_dependency target deps (pos+1)
  in
  parse_name 0 0


       (* ocamldep generates dependencies towards .cmo files for
          .cmi files, even in the case where we are only
          interested in .cmx files !  Problem: if we add two
          dependencies, one to .cmo and one to .cmx, then
          rebuilding any of them will trigger regenerating the
          .cmi, while in fact, only rebuilding both should
          trigger the rebuilding.

          In a general case, what should we do if there is a
          dependency towards a bytecode file (in particular for
          camlp4) when specifying native building ?

          In fact, probably, we want to add the first active
          dependency among a set of dependencies. So, the
          dependency would not be a filename by a list of
          filenames.
       *)

let expanse_dependencies list =
  List.map (fun (target, deps) ->
    if Filename.check_suffix target  ".cmi" then
      (target,
       List.map (fun dep ->
           if Filename.check_suffix dep ".cmo" then
             let cmx = Bytes.of_string dep in
             cmx.[Bytes.length cmx - 1 ] <- 'x';
             let cmx = Bytes.to_string cmx in
             [ dep; cmx ]
           else
             [dep]
         ) deps)
    else
      (target, List.map (fun dep -> [dep]) deps)
    ) list

(* load_dependencies: the old way, i.e. path to files in the load_path *)

let parse_dep_buf = Buffer.create 10000
let load_make_dependencies filename =
  Buffer.clear parse_dep_buf;
  let ic = open_in filename in
  begin
    try
      while true do
   let line = input_line ic in
   Printf.bprintf parse_dep_buf "%s\n%!" line
      done
    with End_of_file -> ()
  end;
  close_in ic;
  parse_dependencies parse_dep_buf

(*
let print_make_dependencies deps =
  List.iter (fun (dep, deps) ->
    Printf.eprintf "%s: " dep;
    List.iter (fun x -> Printf.eprintf "%s " x ) deps;
    Printf.eprintf "\n%!";
  ) deps
*)

let load_make_dependencies filename =
  try
    let deps = load_make_dependencies filename in
(*    print_make_dependencies deps; *)
    deps
  with e ->
    Printf.eprintf "Warning: exception %s in load_make_dependencies\n%!"
      (Printexc.to_string e);
    raise e


let load_dependencies filename =
  expanse_dependencies (load_make_dependencies filename)


(* Another solution:

Use ocamldep -modules toto.ml

When reading, we must keep track of what project this file belongs to.
Then, we can infer from which projects the dependencies are
*)

let print_dependencies deps =
  List.iter (fun (dep, deps) ->
    Printf.eprintf "%s: " dep;
    List.iter (fun list ->
      match list with
     [] -> ()
   | [ x ] -> Printf.eprintf "%s " x
   | [ x; y ] -> Printf.eprintf " (%s|%s) " x y
   | _ -> assert false
    ) deps;
    Printf.eprintf "\n%!";
  ) deps
