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


open BuildTypes
open BuildOCPTypes
open BuildOCamlTypes
open BuildOptions

let list_byte_targets_arg = ref false
let list_asm_targets_arg = ref false

let ocaml_packages = Hashtbl.create 111

let reset () =
  Hashtbl.clear ocaml_packages

let create_package lib opk =
  let envs = [ lib.lib_options ] in

  let lib_archive = BuildValue.get_string_with_default envs "archive" lib.lib_name in
  let lib_stubarchive = BuildValue.get_string_with_default envs "stubarchive" ("ml" ^ lib_archive) in

  let lib = {
    lib = lib;
    lib_opk = opk;

      (* lib_package = pj; *)
    lib_byte_targets = [];
    lib_doc_targets = ref [];
    lib_test_targets = ref [];
    lib_cmo_objects = [];
    lib_bytecomp_deps = [];
    lib_bytelink_deps = [];
    lib_asm_targets = [];
    lib_asm_cmx_objects = [];
    lib_asm_cmxo_objects = [];
    lib_asmcomp_deps = [];
    lib_asmlink_deps = [];
    lib_clink_deps = [];
    lib_modules = ref StringMap.empty;
    lib_internal_modules = StringsMap.empty;
    lib_dep_deps = IntMap.empty;
    lib_includes = None;
    lib_linkdeps = [];
    lib_sources = BuildValue.get_local_prop_list_with_default envs "files" [];
    lib_tests = BuildValue.get_local_prop_list_with_default envs "tests" [];

    lib_build_targets = ref [];
    lib_archive;
    lib_stubarchive;

    lib_requires = List.map (fun dep ->
      let pk2 = dep.dep_project.opk_package in
      let lib2 =
        try
          Hashtbl.find ocaml_packages pk2.package_id
        with Not_found ->
          Printf.eprintf "Unknown dependency %d (%s) of package %S\n%!"
            pk2.package_id
            pk2.package_name
            lib.lib_name;
          BuildMisc.clean_exit 2
      in
      { dep with dep_project = lib2 }
    ) opk.opk_requires;
  }
  in
  Hashtbl.add ocaml_packages lib.lib.lib_id lib;
  if BuildGlobals.verbose 5 then begin
    Printf.eprintf "BuildOCamlGlobals.create_package %S\n" lib.lib.lib_name;
    List.iter (fun (s, _) ->
      Printf.eprintf "  MOD %S\n%!" s;
    ) lib.lib_sources;
  end;
  lib

let get_by_id lib =
  try
    Some (Hashtbl.find ocaml_packages lib.lib_id)
  with Not_found -> None

let get_by_name bc name =
  try
    let lib =
      StringMap.find name bc.packages_by_name
    in
    get_by_id lib
  with Not_found -> None

let make_build_targets lib cin =
  match get_by_id lib with
  | None -> []
  | Some lib ->
    (if cin.cin_bytecode then
      List.map fst lib.lib_byte_targets
    else []) @
      (if cin.cin_native then
          List.map fst lib.lib_asm_targets
       else []) @
      !(lib.lib_build_targets)

let make_doc_targets lib _cin =
   match get_by_id lib with
  | None -> []
  | Some lib -> !(lib.lib_doc_targets)

let make_test_targets lib _cin =
   match get_by_id lib with
  | None -> []
  | Some lib -> !(lib.lib_test_targets)
