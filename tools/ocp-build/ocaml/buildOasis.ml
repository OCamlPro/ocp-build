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

open BuildValue.TYPES

let find_indent line =
  let rec find_indent line i len =
    if i < len then
      match line.[i] with
          ' ' | '\t' -> find_indent line (i+1) len
        | '#' -> (i, true)
        | _ -> (i, false)
    else (i, false)
  in
  let len = String.length line in
  let (indent, comment) = find_indent line 0 len in
  (indent, comment, String.sub line indent (len - indent))

(* From oasis-0.2.1~alpha1%  less src/oasis/OASISRecDescParser.ml LGPL *)
let oasis_lexer = Genlex.make_lexer
  [
        (* Statement *)
    "+:"; "$:"; ":"; "if"; "{"; "}"; "else";
        (* Section *)
    "Flag"; "Library"; "Executable";
    "SourceRepository"; "Test";
    "Document";
        (* Expression *)
    "!"; "&&"; "||"; "("; ")"; "true"; "false"
  ]

type oasis_line =
    Line of string * oasis_line list ref

let read_oasis filename =
  let ic = open_in filename in
  let lines = ref [] in
  try
    let rec read_line stack ic =
      let line = input_line ic in
      let (indent, comment, line) = find_indent line in
      if comment then
        read_line stack ic
      else
        push_line stack ic indent line

    and push_line stack ic indent line =
      match stack with
          [] -> assert false
        | (current_indent, lines) :: previous_stack ->
            if indent = current_indent then begin
              lines := Line (line, ref []) :: !lines;
              read_line stack ic
            end else
              if indent < current_indent then
                push_line previous_stack ic indent line
              else (* indent > current_indent *)
                match !lines with
                    [] -> assert false
                  | Line (_previous_line, new_lines) :: _ ->
                    new_lines := Line (line, ref []) :: !new_lines;
                    let stack = (indent, new_lines) :: stack in
                    read_line stack ic
    in
    read_line [(0, lines)] ic
  with End_of_file ->
    close_in ic;
    !lines

let print_oasis lines =
  let rec print indent lines =
    List.iter (fun line ->
      let Line (s, lines) = line in
      Printf.fprintf stderr "%s%s\n" indent s;
      print (indent ^ "___") !lines
    ) (List.rev lines)
  in
  print "" lines;
  Printf.fprintf stderr "%!"

let merge_line content lines =
  let lines = content :: (List.map (function Line (s, _) -> s) (List.rev !lines)) in
  String.concat " " lines


let split_words orig =
  let s = Bytes.of_string orig in
  for i = 0 to Bytes.length s - 1 do
    match Bytes.get s i with
    | '\r' | '\n' | '\t' | ';' | ',' ->
      s.[i] <- ' '
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '/' | '-'
    | ' ' | '.'
      ->
      ()
    | _ ->
      Printf.kprintf failwith "Illegal word in %S\n%!" orig
  done;
  let s = Bytes.to_string s in
  OcpString.split_simplify s ' '

let split_words_lowercase orig =
  let s = Bytes.of_string orig in
  for i = 0 to Bytes.length s - 1 do
    match Bytes.get s i with
    | '\r' | '\n' | '\t' | ';' | ',' ->
      s.[i] <- ' '
    | 'A'..'Z' -> s.[i] <- Char.lowercase (Bytes.get s i)
    | 'a'..'z' | '0'..'9' | '_' | '/' | '-'
    | ' ' | '.'
      ->
      ()
    | _ ->
      Printf.kprintf failwith "Illegal word in %S\n%!" orig
  done;
  let s = Bytes.to_string s in
  OcpString.split_simplify s ' '

(*
OASISFormat: OASIS format version used to write file _oasis. (mandatory)
Name: Name of the package. (mandatory)
Version: Version of the package. (mandatory)
Synopsis: Short description of the purpose of this package. (mandatory)
Description: Long description of the package purpose.
Authors: Real people that had contributed to the package. (mandatory)
Copyrights: Copyright owners.
Maintainers: Current maintainers of the package.
LicenseFile: File containing the license.
License: DEP-5 license of the package (See DEP-5). (mandatory)
OCamlVersion: Version constraint on OCaml.
FindlibVersion: Version constraint on Finblib.
ConfType: Configuration system.
PreConfCommand: Command to run before configuration.
PostConfCommand: Command to run after configuration.
BuildType: Build system.
PreBuildCommand: Command to run before build.
PostBuildCommand: Command to run after build.
InstallType: Install/uninstall system.
PreInstallCommand: Command to run before install.
PostInstallCommand: Command to run after install.
PreUninstallCommand: Command to run before uninstall.
PostUninstallCommand: Command to run after uninstall.
PreCleanCommand: Command to run before clean.
PostCleanCommand: Command to run after clean.
PreDistcleanCommand: Command to run before distclean.
PostDistcleanCommand: Command to run after distclean.
Homepage: URL of the package homepage.
Categories: URL(s) describing categories of the package.
FilesAB: Files to generate using environment variable substitution.
Plugins: Extra plugins to use.
BuildDepends: Dependencies on findlib packages,
   including internal findlib packages.
BuildTools: Tools required to compile, including internal executables.

Library:
Build: Set if the section should be built.
Install: Set if the section should be distributed.
DataFiles: Comma separated list of files to be installed for run-time.
BuildTools: Tools required to compile, including internal executables.
CSources: C source files.
CCOpt: -ccopt arguments to use when building.
CCLib: -cclib arguments to use when building.
DllLib: -dlllib arguments to use when building.
DllPath: -dllpath arguments to use when building.
ByteOpt: ocamlc arguments to use when building.
NativeOpt: ocamlopt arguments to use when building.

Executable:
Custom: Create custom bytecode executable.

*)

type oasis_package = {
  oasis_filename : string;
  oasis_archive : string;
  oasis_type : BuildOCPTypes.package_type;
  oasis_dirname : string;
  oasis_modules : string list;
  oasis_internal_modules : string list;
  oasis_build_depends : string list;
  oasis_main_is : string list;
  oasis_install : bool;
  oasis_asm : bool;
  oasis_byte : bool;
  oasis_findlib_name : string option;
  oasis_findlib_parent : string option;
}

type oasis_project = {
  opj_name : string;
  opj_packages : oasis_package list;
  opj_build_depends : string list;
}

let parse_package oasis lines =
  Printf.fprintf stderr "parse_package %s\n%!" oasis.oasis_archive;
  try
    let oasis_modules = ref [] in
    let oasis_internal_modules = ref [] in
    let oasis_build_depends = ref [] in
    let oasis_dirname = ref oasis.oasis_dirname in
    let oasis_main_is = ref [] in
    let oasis_install = ref true in
    let oasis_byte = ref true in
    let oasis_asm = ref true in
    let oasis_findlib_name = ref None in
    let oasis_findlib_parent = ref None in

    List.iter (fun line ->
      let Line (s, lines) = line in
      let (header, content) = OcpString.cut_at s ':' in
      match String.lowercase header with

      | "mainis" ->
        let line = merge_line content lines in
        Printf.eprintf  "[%s] MainIs = %S\n%!" oasis.oasis_archive line;
        let modules = split_words line in
        List.iter (fun s -> Printf.eprintf "MODULE %S\n" s) modules;
        Printf.eprintf "\n%!";
        oasis_main_is := !oasis_main_is @ modules

      | "modules" ->
        let line = merge_line content lines in
        Printf.eprintf  "[%s] modules = %S\n%!" oasis.oasis_archive line;
        let modules = split_words line in
        List.iter (fun s -> Printf.eprintf "MODULE %S\n" s) modules;
        Printf.eprintf "\n%!";
        oasis_modules := !oasis_modules @ modules

      | "internalmodules" ->
        let line = merge_line content lines in
        Printf.eprintf "[%s] internalmodules = %S\n%!" oasis.oasis_archive line;
        let modules = split_words line in
        oasis_internal_modules := !oasis_internal_modules @ modules

      | "path" ->
        let line = merge_line content lines in
        Printf.eprintf "[%s] path = %S\n%!" oasis.oasis_archive line;
        begin match split_words line with
          [ subdir ] -> oasis_dirname := Filename.concat !oasis_dirname subdir
          | _ ->
            failwith "Error 'path'\n%!";
        end

      | "findlibname" ->
        let line = merge_line content lines in
        Printf.eprintf "[%s] findlibname = %S\n%!" oasis.oasis_archive line;
        begin match split_words line with
          [ name ] -> oasis_findlib_name := Some name
          | _ ->
            failwith "Error 'findlibname'\n%!";
        end

      | "findlibparent" ->
        let line = merge_line content lines in
        Printf.eprintf "[%s] findlibparent = %S\n%!" oasis.oasis_archive line;
        begin match split_words line with
          [ name ] -> oasis_findlib_parent := Some name
          | _ ->
            failwith "Error 'findlibparent'\n%!";
        end

      | "install" ->
        let line = merge_line content lines in
        Printf.eprintf "[%s] install = %S\n%!" oasis.oasis_archive line;
        begin match split_words_lowercase line with
          [ "true" ] -> oasis_install := true
          | [ "false" ] -> oasis_install := false
          | _ ->
            failwith "Error 'install'\n%!";
        end

      | "compiledobject" ->
        let line = merge_line content lines in
        Printf.eprintf "[%s] compiledobject = %S\n%!" oasis.oasis_archive line;
        begin match split_words_lowercase line with
            [ "best" ] -> ()
          | [ "byte" ] -> oasis_asm := false
          | [ "native" ] -> oasis_byte := false
          | _ ->
            failwith "Error compiledobject\n%!";
        end

      | "builddepends" ->
        let line = merge_line content lines in
        Printf.eprintf "[%s] REQUIRES = %s\n%!" oasis.oasis_archive line;
        oasis_build_depends := !oasis_build_depends @ split_words line

      | _ ->
        Printf.eprintf "[%s]Discarding line [%s]\n%!" oasis.oasis_archive s
    ) (List.rev lines);
    List.iter (fun s -> Printf.eprintf "MODULE1 %S\n" s) !oasis_modules;
    let oasis = {
      oasis with
      oasis_modules = !oasis_modules;
      oasis_internal_modules = !oasis_internal_modules;
      oasis_build_depends = !oasis_build_depends;
      oasis_dirname = !oasis_dirname;
      oasis_main_is = !oasis_main_is;
      oasis_install = !oasis_install;
      oasis_byte = !oasis_byte;
      oasis_asm = !oasis_asm;
      oasis_findlib_parent = !oasis_findlib_parent;
      oasis_findlib_name = !oasis_findlib_name;

    }
    in
    List.iter (fun s -> Printf.eprintf "MODULE2 %S\n" s) oasis.oasis_modules;
    Some oasis
  with Failure s ->
    Printf.eprintf "Warning: in package %S, error:\n" oasis.oasis_archive;
    Printf.eprintf "  %s\n%!" s;
    None

let empty_oasis = {
  oasis_filename = "";
  oasis_archive = "";
  oasis_dirname = "";
  oasis_type = LibraryPackage;
  oasis_modules = [];
  oasis_internal_modules = [];
  oasis_build_depends = [];
  oasis_main_is = [];
  oasis_install = true;
  oasis_byte = true;
  oasis_asm = true;
  oasis_findlib_name = None;
  oasis_findlib_parent = None;
}

let parse_oasis oasis_filename lines =
  let open Genlex in
  let empty_oasis = {
    empty_oasis with
    oasis_filename;
    oasis_dirname = Filename.dirname oasis_filename;
  } in

  let opj_name = ref "" in
  let opj_packages = ref [] in
  let opj_build_depends = ref [] in

  List.iter (fun line ->
    let Line (s, lines) = line in
    try
      let tokens = OcpGenlex.tokens_of_string_exn oasis_lexer s in
      let oasis =
        match tokens with
          [ Ident "Name" ; Kwd ":" ; (String name | Ident name) ] ->
          opj_name := name;
          None

(*
      | Ident "BuildDepends"; Kwd ":"; tail ->
        let line = merge_line content lines in
        Printf.eprintf "[%s] REQUIRES = %s\n%!" oasis.oasis_archive line;
        oasis_build_depends := !oasis_build_depends @ split_words line
*)


        | [ Kwd "Library"; (String oasis_archive | Ident oasis_archive) ] ->
          let oasis = {
            empty_oasis with
            oasis_archive;
            oasis_type = LibraryPackage;
          } in
          parse_package oasis !lines
        | [ Kwd "Executable"; (String oasis_archive | Ident oasis_archive) ] ->
          let oasis = {
            empty_oasis with
            oasis_archive;
            oasis_type = ProgramPackage;
          } in
          parse_package oasis !lines

        | _ -> None
      in
      match oasis with
        None -> ()
      | Some oasis ->
        opj_packages := oasis :: !opj_packages
    with _ ->
      Printf.fprintf stderr "Discarding line [%s]\n%!" s
  ) (List.rev lines);

  {
    opj_name = !opj_name;
    opj_packages = !opj_packages;
    opj_build_depends = !opj_build_depends;
  }

open BuildOCPTypes

let load_project pj filename =
  let lines = read_oasis filename in
  print_oasis lines;
  let opj = parse_oasis filename lines in

  let po = BuildValue.empty_env in
  let po = BuildValue.set_bool po "sort" true in

(*  let _local_packages = ref StringMap.empty in *)
(*
  List.iter (fun oasis ->
    if not oasis.oasis_install then
      let name = oasis_name opj oasis in
      let uniq_name = Printf.eprintf
  ) opj.opj_packages;
*)

  List.iter (fun oasis ->
    Printf.eprintf "oasis_archive = %S\n%!" oasis.oasis_archive;

    if oasis.oasis_modules <> [] || oasis.oasis_internal_modules <> [] ||
       oasis.oasis_main_is <> [] then

      let name =
        if oasis.oasis_install then
          match oasis.oasis_findlib_parent, oasis.oasis_findlib_name with
            None, None -> oasis.oasis_archive
          | None, Some name -> name
          | Some parent, Some name -> Printf.sprintf "%s.%s" parent name
          | Some _, None -> assert false
        else
          match oasis.oasis_type with
          | LibraryPackage ->
            Printf.sprintf "%s-library-%s" opj.opj_name oasis.oasis_archive
          | ProgramPackage ->
            Printf.sprintf "%s-program-%s" opj.opj_name oasis.oasis_archive
          | TestPackage -> assert false
          | ObjectsPackage -> assert false
          | SyntaxPackage -> assert false
          | RulesPackage -> assert false
      in
      Printf.eprintf "  name = %S\n%!" name;

      let requires =
        oasis.oasis_build_depends @ opj.opj_build_depends in
      let po = BuildValue.set po "requires"
        (VList (List.map (fun s ->
          let link =
            if Filename.check_suffix s ".syntax" then false else true in
          VTuple [VString (s,StringRaw);
                  VObject (BuildValue.set_bool BuildValue.empty_env "tolink" link)]
         ) requires)) in

      let pk = BuildOCP.new_package (BuildValue.noloc oasis.oasis_filename)
        pj name oasis.oasis_dirname
          oasis.oasis_filename [oasis.oasis_filename, None (* matters only for non-installed packages *)
                           ] oasis.oasis_type po in
      pk.package_source_kind <- "oasis";

(* TODO
      let external_options = [] in
      let internal_options = set
        (OptionBoolSet ("install", false)) ::
          external_options in
      pk.package_raw_files <-
        List.map (fun s -> (s, external_options)) oasis.oasis_modules @
          List.map (fun s -> (s, internal_options)) oasis.oasis_internal_modules @
          List.map (fun s -> (s, external_options)) oasis.oasis_main_is;
      assert false;
      let po = StringMap.add "install" (OptionBool oasis.oasis_install) po in
      let po = StringMap.add "has_byte" (OptionBool oasis.oasis_byte) po in
      let po = StringMap.add "has_asm" (OptionBool oasis.oasis_asm) po in
      let po = StringMap.add "archive" (OptionList [ oasis.oasis_archive]) po in
*)

  ) opj.opj_packages;
  0
