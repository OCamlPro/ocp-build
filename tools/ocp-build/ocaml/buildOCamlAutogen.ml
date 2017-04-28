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


open BuildOCPTypes

let create_package name ptype dirname_t =
  let file_t = FileGen.add_basename dirname_t (name ^ ".ocp") in

  let map = ref StringMap.empty in
  let files = FileDir.list dirname_t in
  List.iter (fun file ->
    try
      let modfile =
        let modfile = Bytes.of_string file in
        modfile.[0] <- Char.uppercase (Bytes.get modfile 0);
        Bytes.to_string modfile
      in
      let basename, ext = FileString.cut_at_last_extension modfile in
      let modfile = basename ^ "." ^ ext in
      map := StringMap.add modfile (file, basename, ext) !map
    with Not_found -> ()
  ) files;

  let map = !map in
  let files = ref [] in
  StringMap.iter (fun _modfile (file, basename, ext) ->
    match ext with
        "ml" ->
          if not (StringMap.mem (basename ^ ".mll") map) &&
            not (StringMap.mem (basename ^ ".mly") map) then
            files := file :: !files
      | "mli" ->
        if not (StringMap.mem (basename ^ ".mly") map) &&
          not (StringMap.mem (basename ^ ".ml") map) then
          files := file :: !files
      | "mly"
      | "mll"
      | "c" ->
        files := file :: !files
      | _ -> ()
  ) map;
  let source_files = !files in

  let oc = FileGen.open_out file_t in
  Printf.fprintf oc "begin %s \"%s\"\n"
    (match ptype with
    | ProgramPackage -> "program"
    | ObjectsPackage -> "objects"
    | LibraryPackage -> "library"
    | SyntaxPackage -> "syntax"
    | TestPackage -> "test"
    | RulesPackage -> "rules"
    (*      | ProjectToplevel -> "toplevel" *)
    )
    name;
  Printf.fprintf oc "   sort = true\n";
  Printf.fprintf oc "   files = [ %s ]\n" (match source_files with
      [] -> ""
    | _ -> "\"" ^ String.concat "\" \"" source_files ^ "\"");
  Printf.fprintf oc "   requires = [ ]\n";
  Printf.fprintf oc "end\n";
  close_out oc
