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


open StringCompat
open BuildOCPTypes
open BuildValue.Types

let string_of_package_type = function
  | ProgramPackage -> "ProgramPackage"
  | TestPackage -> "TestPackage"
  | LibraryPackage -> "LibraryPackage"
  | ObjectsPackage -> "ObjectsPackage"
  | SyntaxPackage -> "SyntaxPackage"
  | RulesPackage -> "RulesPackage"

let bprint_string b indent s = Printf.bprintf b "%S" s

let rec bprint_plist b indent list =
  match list with
    [] -> Printf.bprintf b "%s[]\n" indent
  | list ->
    Printf.bprintf b "%s[\n" indent;
    List.iter (fun (s, env) ->
      Printf.bprintf b "%s  %S\n" indent s;
      if env <> BuildValue.empty_env then begin
        Printf.bprintf b "%s  (\n" indent;
        bprint_env b (indent ^ "  ") env;
        Printf.bprintf b "%s  )\n" indent;
      end
    ) list;
    Printf.bprintf b "%s]\n" indent;
    ()

and bprint_value b indent v =
  match v with
  | VString s -> Printf.bprintf b "%S" s
  | VBool bool -> Printf.bprintf b "%b" bool
  | VInt int -> Printf.bprintf b "%d" int
  | VPair (v1, v2) ->
    bprint_value b indent v1;
    Printf.bprintf b ", ";
    bprint_value b indent v2
  | VObject env ->
    Printf.bprintf b "{\n";
    bprint_env b indent env;
    Printf.bprintf b "}"
  | VList [] ->
    Printf.bprintf b "[]"
  | VList list ->
    Printf.bprintf b "[\n";
    List.iter (fun v ->
      Printf.bprintf b "%s" indent;
      bprint_value b indent v;
      Printf.bprintf b "\n") list;
    Printf.bprintf b "]"

and bprint_env b indent env =
  BuildValue.iter (fun var v ->
    Printf.bprintf b "%s%s -> " indent var;
    bprint_value b (indent^"  ") v;
    Printf.bprintf b "\n"
  ) env

let stringMap printer b indent array =
  Printf.bprintf b "{{\n";
  let indent2 = indent ^ "  " in
  StringMap.iter (fun s x ->
    Printf.bprintf b "%s%S -> " indent2 s;
    printer b indent2 x;
    Printf.bprintf b ";\n"
  ) array;
  Printf.bprintf b "%s}}" indent

let intMap printer b indent array =
  Printf.bprintf b "{{\n";
  let indent2 = indent ^ "  " in
  IntMap.iter (fun s x ->
    Printf.bprintf b "%s%d -> " indent2 s;
    printer b indent2 x;
    Printf.bprintf b ";\n"
  ) array;
  Printf.bprintf b "%s}}" indent

let array_of printer b indent array =
  Printf.bprintf b "[|\n";
  let indent2 = indent ^ "  " in
  Array.iter (fun x ->
    Printf.bprintf b "%s" indent2;
    printer b indent2 x;
    Printf.bprintf b ";\n"
  ) array;
  Printf.bprintf b "%s|]" indent

let list_of printer b indent array =
  let indent2 = indent ^ "  " in
  Printf.bprintf b "[\n";
  List.iter (fun x ->
    Printf.bprintf b "%s" indent2;
    printer b indent2 x;
    Printf.bprintf b ";\n"
  ) array;
  Printf.bprintf b "]"

let package_dependency printer b indent pd =
  let indent2 = indent ^ "  " in
  let indent4 = indent2 ^ "  " in
  Printf.bprintf b "{\n" ;
  Printf.bprintf b "%s  dep_project = " indent;
  printer b indent2 pd.dep_project;
  Printf.bprintf b ";\n";
  Printf.bprintf b "%s  dep_link = %b;\n" indent pd.dep_link;
  Printf.bprintf b "%s  dep_syntax = %b;\n" indent pd.dep_syntax;
  Printf.bprintf b "%s  dep_optional = %b;\n" indent pd.dep_optional;
  Printf.bprintf b "%s  dep_options = {{\n" indent;
  bprint_env b indent4 pd.dep_options;
  Printf.bprintf b "  %s}};\n" indent;
  Printf.bprintf b "%s}" indent

let package_uid b indent p =
  Printf.bprintf b "{ package with package_uid = \"%s_%d\" }"
    p.package_name p.package_id

let package package_info b indent p =
  let indent2 = indent ^ "  " in
  let indent4 = indent2 ^ "  " in
  Printf.bprintf b "{\n";
  Printf.bprintf b "%s  package_uid = \"%s_%d\";\n" indent
    p.package_name p.package_id;
  Printf.bprintf b "%s  package_name = %S;\n" indent p.package_name;
  Printf.bprintf b "%s  package_dirname = %S;\n" indent p.package_dirname;
  Printf.bprintf b "%s  package_options = {{\n" indent;
  bprint_env b indent4 p.package_options;
  Printf.bprintf b "  %s}};\n" indent;
  Printf.bprintf b "%s  package_source_kind = %S;\n" indent p.package_source_kind;
  if p.package_provides <> p.package_name then
    Printf.bprintf b "%s  package_provides = %S;\n" indent p.package_provides;

  if p.package_version <> "[distributed with Ocaml]" then
    Printf.bprintf b "%s  package_version = %S;\n" indent p.package_version;
  if p.package_loc <> -1 then
    Printf.bprintf b "%s  package_loc = %d;\n" indent p.package_loc;
  Printf.bprintf b "%s  package_filename = %S;\n" indent p.package_filename;
  begin match p.package_auto with
    | None -> ()
    | Some s ->
      Printf.bprintf b "%s  package_auto = Some %S;\n" indent s;
  end;
  Printf.bprintf b "%s  package_type = %s;\n" indent
    (string_of_package_type p.package_type);
  Printf.bprintf b "%s  package_filenames = " indent;
  list_of (fun b indent (s, digest) ->
    Printf.bprintf b "%s%S, %s" indent s
      (match digest with
       | None -> "None"
       | Some digest -> "Some _")
  ) b indent p.package_filenames;
  Printf.bprintf b ";\n";
  Printf.bprintf b "%s  package_id = %d;\n" indent p.package_id;
  package_info b indent p.pi;
(*
  mutable package_requires : package package_dependency list;
  mutable package_requires_map : package package_dependency IntMap.t;
*)
  Printf.bprintf b "%s}\n" indent

let package_info b indent pi =
  let indent2 = indent ^ "  " in
  Printf.bprintf b "%s  package_validated = %b;\n" indent pi.package_validated;
  Printf.bprintf b "%s  package_added = %b;\n" indent pi.package_added;
(*  package_node : LinearToposort.node; *)
  Printf.bprintf b "%s  package_deps_map = " indent;
  stringMap (package_dependency bprint_string) b indent2 pi.package_deps_map;
  Printf.bprintf b ";\n";
  Printf.bprintf b "%s  package_requires = " indent;
  list_of (package_dependency package_uid) b indent2 pi.package_requires;
  Printf.bprintf b ";\n";
  Printf.bprintf b "%s  package_requires_map = " indent;
  intMap (package_dependency package_uid) b indent2 pi.package_requires_map;
  Printf.bprintf b ";\n";
  ()

let final_package = package package_info

let string_x_package_list b indent (s, plist) =
  Printf.bprintf b "%s%S, " indent s;
  let indent2 = indent ^ "  " in
  list_of final_package b indent2 plist

let package_x_package_x_package b indent (p1,p2,p3) =
  package_uid b indent p1;
  Printf.bprintf b ",\n%s" indent;
  package_uid b indent p2;
  Printf.bprintf b ",\n%s" indent;
  package_uid b indent p3

let project b indent p =
  let indent2 = indent ^ "  " in
  Printf.bprintf b "{\n";
  Printf.bprintf b "%s  project_disabled = " indent;
  array_of final_package b indent p.project_disabled;
  Printf.bprintf b ";\n%s  project_incomplete = " indent;
  array_of final_package b indent p.project_incomplete;
  Printf.bprintf b ";\n%s  project_sorted = " indent;
  array_of final_package b indent p.project_sorted;
  Printf.bprintf b ";\n%s  project_missing = " indent;
  list_of string_x_package_list b indent p.project_missing;
  Printf.bprintf b ";\n%s  project_conflicts = " indent;
  list_of package_x_package_x_package b indent2 p.project_conflicts;
  Printf.bprintf b " }\n"

let string_of_project p =
  let b = Buffer.create 1111 in
  project b "  " p;
  Buffer.contents b


let eprint_project label p =
  Printf.eprintf "%s:\n%s\n" label (string_of_project p)

let string_of_package package_info p =
  let b = Buffer.create 1111 in
  package package_info b "  " p;
  Buffer.contents b
