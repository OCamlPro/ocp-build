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

let w_SyntaxDepDeclaredAsNotSyntax w (lib_name, tool_name, pk_name) =
  BuildWarnings.wprintf w
    "Warning: in package %S, %s_requires: dependency %S not declared as syntax"
    lib_name tool_name pk_name

let w_SyntaxDepNotDeclared w (lib_name, tool_name, pk_name) =
  BuildWarnings.wprintf w
    "Warning: in package %s, %s_requires dependency %S not declared\n%!"
    lib_name tool_name pk_name


(* TODO: generate a

begin syntax "toto:syntax"
  requires = [ "toto" ]
end
 automatically from
begin library "toto"
  ...
end
*)


(* let verbose = DebugVerbosity.verbose ["O"] "BuildOCamlSyntaxes" *)

(* TODO:

   Currently, there is no verification that all the dependencies
   appearing in pp_requires also appear in requires. Actually,
   pp_requires could be automatically added.

   We could simplify this by:

   ppflags = [ "" ]

   requires =

   syntax = [ "pa_dyntype" ]

   A syntax MUST either depend on another syntax (requires = [ "toto" ])
   where toto is a syntax, OR set the "pp_master" option. If we depend
   on a package, and this package depends on another syntax, it does
   not matter.
*)

open BuildMisc
open BuildEngineTypes
open BuildEngineRules
open BuildValue.TYPES
open BuildTypes
open BuildOCamlTypes
open BuildOCamlVariables
open BuildOCamlMisc

let verbose = OcpDebug.verbose_function ["B"; "BuildOCamlSyntaxes"]

(* [execution_dependencies lib kind]
   * [lib : BuildOCamlTypes.ocaml_package]: package
   * [kind : string] either "byte" or "asm"

   Lookup the set of [build_file] needed to be able to use this package.
*)

let execution_dependencies lib kind =
  if lib.lib_opk.opk_installed then [] else
  let pk_name = lib.lib.lib_name in
  try
    match lib.lib.lib_type with
    | TestPackage -> assert false
    | ProgramPackage ->
      let exe_ext = if kind = "byte" then byte_exe else asm_exe in
      [find_dst_file lib.lib.lib_dst_dir (pk_name ^ exe_ext)]
   | LibraryPackage ->
      List.map (fun (obj,kind) ->
          match kind with
          | CMA | CMO -> obj
          | _ -> assert false)
               (match kind with
                | "byte" -> lib.lib_byte_targets
                | "asm" -> lib.lib_asm_targets
                | _ -> assert false)

    | ObjectsPackage ->
      if kind = "byte" then
        List.fold_right (fun (obj,kind) cmos ->
          match kind with
          | CMO -> obj :: cmos
          | _ -> cmos
        ) lib.lib_byte_targets []
      else
        assert false (* TODO *)
    | RulesPackage -> assert false
    | SyntaxPackage ->
      (* TODO: what shall we do ? *)
      []
  with NoSuchFileInDir (filename, dirname) as e ->
    Printf.fprintf stderr "BuildOCamlRules.find_dst_file: could not find %S in %S\n%!"
      filename dirname;
    raise e

(* This can only work if the package already appears in requires.
For camlp4 and camlp5, we should:
- at the package creation, add all camlp4/camlp5 packages to 'requires',
with the nolink option.
 *)

(* [get_tool_require w_set toolname lib tool_require]
   * [w_set : BuildWarnings.set]: set of warnings
   * [toolname : string]: name of tool, only used in error messages
   * [lib : BuildOCamlTypes.ocaml_package]: current package
   * [tool_require : string]: tool requirement

   [tool_require] must be of the form "package:kind" with [kind] either
     "asm" or "byte". The [package] is looked up in the set of existing
     packages (reason why it should be in "requires" too).

*)

let get_tool_require w tool_name lib s =
  let bc = lib.lib.lib_builder_context in
  let pk_name, kind = OcpString.cut_at s ':' in
  let _exe_ext =
    match kind with
      "asm" -> asm_exe
    | "byte" -> byte_exe
    | "" ->
      Printf.eprintf "Error: package %s\n%!" lib.lib.lib_name;
      Printf.eprintf "Error: %s_requires: you must specify either kind 'asm' or 'byte' for package '%s'\n%!" tool_name pk_name;
      BuildMisc.clean_exit 2
    | _ ->
      Printf.fprintf stderr "Error: package %s\n%!" lib.lib.lib_name;
      Printf.fprintf stderr "Error: %s_requires option contains unknown kind [%s] for package '%s'\n%!" tool_name kind pk_name;
      clean_exit 2
  in
  let pk = try
    StringMap.find pk_name bc.packages_by_name
  with Not_found ->
    Printf.fprintf stderr "Error: package %s\n%!" lib.lib.lib_name;
    Printf.fprintf stderr "Error: %s_requires: unknown package '%s'\n%!" tool_name pk_name;
    clean_exit 2
  in
  let declared = ref false in
  List.iter (fun dep ->
    let olib = dep.dep_project in
    if olib == lib then begin
      if not dep.dep_syntax then begin
        w_SyntaxDepDeclaredAsNotSyntax w (lib.lib.lib_name, tool_name, pk_name)
(*        print_warnings [
        Printf.sprintf "package %S" lib.lib.lib_name;
          Printf.sprintf "%s_requires: dependency %S not declared as syntax" tool_name pk_name ] *)
      end;

      declared := true
    end
  ) lib.lib_requires;
  if not !declared then begin
    w_SyntaxDepNotDeclared w (lib.lib.lib_name, tool_name, pk_name);
  end;
  match BuildOCamlGlobals.get_by_id pk with
  | None ->
    Printf.eprintf "Warning: expected %S to be an OCaml package\n%!"
      pk.lib_name;
    exit 2

  | Some lib -> execution_dependencies lib kind

let get_tool_requires w tool_name lib tool_requires =
  List.flatten (List.map (get_tool_require w tool_name lib) tool_requires)

(* TODO: for syntax extensions:
1/ check the "syntax" attribute.
2/ find the corresponding package. Verify that it was provided in the
   "requires" or "syntaxes".
3/ if it is a program, add a dependency to the bytecode version of it,
   set "pp" to be [ "%{program_DST_DIR}%/program.byte" ] + ppflags
4/ if it is a set of libraries, check the libraries to find for which
   tool they are a plugin (one of them should contain an attribute
   "plugin_for"). Then, build the corresponding command and add it
   to "pp".
*)

let add_pp_requires r pp =
  List.iter (fun file -> add_rule_source r file) pp.pp_requires

(* [get_pp kind w_set lib basename options]
   * [kind : string] special: asm, byte, dep, mli
   * [w_set : BuildWarnings.set]
   * [lib : BuildOCamlTypes.ocaml_package]
   * [basename : string]
   * [options : BuildValue.TYPES.env]
  returns [BuildOCamlTypes.pp] {pp_flags,pp_option,pp_requires}
*)

let get_pp special w lib basename options =
  let options = options :: lib.lib_opk.opk_options in
(*  Printf.eprintf "get_pp %S\n%!" lib.lib.lib_name; *)
  let pp_flags =
    List.map (fun s -> S s)
      (ppflags_option.get options) in
  match  syntax_option.get options with
  |  [] ->
    let pp_requires =  pp_requires_option.get options in
    let pp_option = BuildOCamlVariables.get pp_option special options in
    let pp_requires = get_tool_requires w "pp" lib pp_requires in

    {
      pp_flags = pp_flags;
      pp_option = pp_option;
      pp_requires = pp_requires;
    }

  | syntaxes ->
    let bc = lib.lib.lib_builder_context in

    let syntaxes = List.map (fun s ->
        let pksy =
          try
            StringMap.find s bc.packages_by_name
          with Not_found ->
            Printf.eprintf "Error with package %S, file %S:\n"
              lib.lib.lib_name basename;
            Printf.eprintf
              "   Syntax %S could not be found among existing packages\n%!" s;
            clean_exit 2
        in
        match BuildOCamlGlobals.get_by_id pksy with
        | None -> assert false
        | Some pksy ->
        let found = ref false in
        List.iter (fun dep ->
          if dep.dep_syntax && dep.dep_project == pksy then
            found := true
        ) lib.lib_requires;
        if not !found then begin
          Printf.eprintf "Error with package %S, file %S:\n"
            lib.lib.lib_name basename;
          Printf.eprintf
            "   Syntax %S was not in 'requires' of package.\n%!" pksy.lib.lib_name;
        end;
        pksy
      ) syntaxes in
    let (pp, lib_requires) =
      match syntaxes with
      | [] -> assert false
      | [ { lib = {lib_type = ProgramPackage} } as pp ] -> (pp, [])
      | [ pksy ] ->

        if verbose 3 then begin
          Printf.eprintf "Package %S, file %S, syntax %S:\n"
            lib.lib.lib_name basename pksy.lib.lib_name;
          List.iter (fun dep ->
            let olib = dep.dep_project in
            Printf.eprintf "  dep: %s %S\n%!"
              (BuildOCP.string_of_package_type olib.lib.lib_type) olib.lib.lib_name
          ) pksy.lib_requires;
        end;

        let preprocessor = ref [] in
        List.iter (fun dep ->
          match dep.dep_project.lib.lib_type with
            ProgramPackage ->
            preprocessor := dep.dep_project :: !preprocessor
          | TestPackage -> assert false
          | LibraryPackage
          | ObjectsPackage
          | RulesPackage
          | SyntaxPackage -> ()
        ) pksy.lib_requires;
        begin
          match !preprocessor with
          | [] ->
            Printf.eprintf "Error with package %S, file %S:\n"
              lib.lib.lib_name basename;
            Printf.eprintf
              "   One of the syntax must specify the preprocessor to use.\n%!";
            clean_exit 2
          | _ :: _ :: _ ->
            Printf.eprintf "Error with package %S, file %S:\n"
              lib.lib.lib_name basename;
            Printf.eprintf
          "   Only one preprocessor should be specified within syntaxes.\n%!";
            clean_exit 2

          | [ pp ] ->
            (pp, pksy.lib_requires)
        end

      | [s1;s2] ->
        begin
          match s1, s1.lib.lib_type, s2, s2.lib.lib_type with
          | _, ProgramPackage, _pksy, ProgramPackage ->
            Printf.eprintf "Error with package %S, filw %S:\n"
              lib.lib.lib_name basename;
            Printf.eprintf "   Only one preprocessor can be specified.\n%!";
            clean_exit 2

          | pp, ProgramPackage, pksy, _
          | pksy, _, pp, ProgramPackage ->
            (pp, pksy.lib_requires)

          | _ ->
            Printf.eprintf "Error with package %S, filw %S:\n"
              lib.lib.lib_name basename;
            Printf.eprintf "   Only one syntax can be specified.\n%!";
            clean_exit 2
        end
      | _:: _ :: _ ->
        Printf.eprintf "Error with package %S, filw %S:\n"
          lib.lib.lib_name basename;
        Printf.eprintf "   Only one syntax can be specified.\n%!";
        clean_exit 2

    in
    let pp_args = ref [] in
    let pp_option = ref [] in
    let pp_requires = ref [] in

    if pp.lib_opk.opk_installed then
      pp_option := [ pp.lib.lib_name ]
    else begin
      pp_requires := [ find_dst_file lib.lib.lib_dst_dir (pp.lib.lib_name ^ ".byte") ];
      pp_option := [
        Printf.sprintf "%%{%s_DST_DIR}%%/%s.byte" pp.lib.lib_name pp.lib.lib_name
      ];
    end;

    let already_linked_map = ref StringSet.empty in
    (* remove libraries that are already included in the preprocessor *)
    List.iter (fun dep ->
      if dep.dep_link then
        already_linked_map := StringSet.add dep.dep_project.lib.lib_name
            !already_linked_map
    ) pp.lib_requires;

    List.iter (fun dep ->
      let plib = dep.dep_project in
        if plib != pp
        && dep.dep_link
      && not (StringSet.mem plib.lib.lib_name !already_linked_map)
        && (plib.lib_sources <> [] || plib.lib_opk.opk_installed)
      then begin
        pp_requires := (execution_dependencies plib "byte") @ !pp_requires;
        if not plib.lib_meta then
          pp_args := !pp_args @
                     [ S "-I"; BD plib.lib.lib_dst_dir ] @
                     (match plib.lib.lib_type with
                      | ProgramPackage -> assert false
                      | TestPackage -> assert false
                      | ObjectsPackage ->
                        List.map (fun s -> BF s)
                          (execution_dependencies plib "byte")
                      | LibraryPackage ->
                         List.map (fun (obj,kind) ->
                             match kind with
                             | CMO | CMA -> BF obj
                             | _ -> assert false)
                                  (plib.lib_byte_targets)
                      | SyntaxPackage -> []
                      | RulesPackage -> []
                     )
      end
    ) lib_requires;

    {
      pp_flags = !pp_args @ pp_flags;
      pp_option = !pp_option;
      pp_requires = !pp_requires;
    }
