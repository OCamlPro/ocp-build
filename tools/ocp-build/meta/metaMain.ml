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

let _ =
  for i = 1 to Array.length Sys.argv - 1 do
    let file = Sys.argv.(i) in
    try
      Printf.fprintf stderr "Parsing file %S\n%!" file;
      let p = MetaFile.meta_of_file file in
      Printf.printf "%s\n-----------------------------------------------\n%!"
        (MetaFile.string_of_meta p);
    with e ->
      Printf.fprintf stderr "MetaMain: Exception %S while parsing %S\n%!"
        (Printexc.to_string e) file
  done
