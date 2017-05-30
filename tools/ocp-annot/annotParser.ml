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

open StringCompat

type position = int * int * int (* (line number, line pos, char pos) *)

type location = string * position * position

type kind =
  Type of string list
| Ident of string

type ident =
  Def of string * location (* scope *)
| ExtRef of string
| IntRef of string * location (* definition *)

type annot_file = {
  annot_filenames : string list;
  annot_infos : (location * kind list) list;
}

let error fn s =
  Printf.eprintf "Error: unexpected argument %S to function %s\n%!"
    s fn;
  exit 2

let int_of_string s =
  try
    int_of_string s
  with _ -> error "int_of_string" s

let parse_pos pos =
  match OcpString.split pos ' ' with
    "" :: line :: char1 :: char2 :: ( [] | [ "" ]) ->
      (int_of_string line, int_of_string char1, int_of_string char2)
  | _ -> error "parse_pos" pos

let parse_location fileset line =
  (*  Printf.eprintf "parse_location %S\n%!" line; *)
  match OcpString.split line '"' with
    [ ""; file1; pos1; file2; pos2 ] ->
      let pos1 = parse_pos pos1 in
      let (_,_,endpos) as pos2 = parse_pos pos2 in
      if endpos >= 0 && not (StringSet.mem file1 !fileset) then
        fileset := StringSet.add file1 !fileset;
      assert (file1 = file2);
      (file1, pos1, pos2)
  | _ -> error "parse_location" line

let parse_ident line =
  match OcpString.split line '"' with
    [ reference; file1; pos1; file2; pos2 ] ->
      let pos1 = parse_pos pos1 in
      let pos2 = parse_pos pos2 in
      let location = (file1, pos1, pos2) in
      begin match OcpString.split reference ' ' with
        [ "";""; kind; ident;"" ] ->
          begin
            match kind with
            | "def" -> Def (ident, location)
            | "int_ref" -> IntRef (ident, location)
            | _ -> error "parse_reference.kind" kind;
          end
      | _ -> error "parse_reference.reference" reference
      end
  | [ reference ] ->
      begin match OcpString.split reference ' ' with
        [ "";""; "ext_ref"; ident ] ->  ExtRef ident
      | [ "";""; kind; pre; infix; ")"] -> ExtRef (pre ^ infix ^ ")")
      | _ -> error "parse_reference.simple_reference" reference
      end
  | _ -> error "parse_reference" line


let parse_file filename =
  let fileset = ref StringSet.empty in
  let lines = FileLines.read_file filename in
  let rec iter lines locations =
    match lines with
      [] | [ "" ] -> locations
    | line :: lines ->
      let location = parse_location fileset line in
      iter2 lines locations location []

  and iter2 lines locations location infos =
    match lines with
    | [] -> locations
    | "type(" :: lines ->
      iter3 lines locations location infos []
    | "ident(" :: reference :: ")" :: lines ->
      (*      let _ = parse_reference reference in *)
      iter2 lines locations location (Ident reference :: infos)
    | line :: lines ->
      let locations = (location, infos) :: locations in
      let location = parse_location fileset line in
      iter2 lines locations location []


  and iter3 lines locations location infos prev_lines =
    match lines with
    | [] -> assert false
    | ")" :: lines ->
      iter2 lines locations location ( (Type (List.rev prev_lines)) :: infos)
    | line :: lines ->
      iter3 lines locations location infos (line :: prev_lines)
  in
  let annot_infos = iter lines [] in
  let annot_filenames = StringSet.to_list !fileset in
  { annot_infos; annot_filenames }
