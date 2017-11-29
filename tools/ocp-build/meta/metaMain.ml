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
open MetaTypes

let list_all files =
  List.iter (fun file ->
    try
      Printf.fprintf stderr "Parsing file %S\n%!" file;
      let p = MetaFile.meta_of_file file in
      Printf.printf "%s\n-----------------------------------------------\n%!"
        (MetaFile.string_of_meta p);
    with e ->
      Printf.fprintf stderr "MetaMain: Exception %S while parsing %S\n%!"
        (Printexc.to_string e) file
    ) files

let hash_all files =
  let h = Hashtbl.create 111 in
  let rec hash_packages file p =
    List.iter (fun (name, p) ->
        hash_packages (file ^ "." ^ name) p
      ) p.p_packages;
    let p = {
        p_packages = [];
        p_parent = None;
        p_variables = p.p_variables;
      } in
    let pp = {
        p with
        p_variables = StringMap.map (fun v ->
                          let var_assigns = List.map (fun (precond,_) ->
                                                (precond,"_")) v.var_assigns in
                          let var_additions = List.map (fun (precond,_) ->
                                                  (precond,"_")) v.var_additions
                          in
                          { v with var_assigns; var_additions }
                        ) p.p_variables
      } in
    if not (Hashtbl.mem h pp) then
      Hashtbl.add h pp (file, { p with p_parent = None })
  in
  List.iter (fun file ->
    try
      Printf.fprintf stderr "Parsing file %S\n%!" file;
      let p = MetaFile.meta_of_file file in
      hash_packages file p
    with e ->
      Printf.fprintf stderr "MetaMain: Exception %S while parsing %S\n%!"
        (Printexc.to_string e) file
    ) files;
  Hashtbl.iter (fun p (name,pp) ->
      Printf.printf
        "%s\n%s\n%s\n-----------------------------------------------\n%!"
        name
        (MetaFile.string_of_meta p)
        (MetaFile.string_of_meta pp)
    ) h

let () =
  let files = ref [] in
  let hash = ref false in
  Arg.parse [
      "--hash", Arg.Set hash, " Only print exemplary packages"
    ] (fun file -> files := file :: !files)
            " load META files";
  let files = List.rev !files in
  if !hash then
    hash_all files
  else
    list_all files
