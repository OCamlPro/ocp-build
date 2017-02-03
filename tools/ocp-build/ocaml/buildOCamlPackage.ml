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
open BuildOCamlTypes
open BuildValue.Types

let verbose = DebugVerbosity.verbose [ "B"; "BP" ] "BuildOCamlPackage"

exception OCamlPackage of ocaml_description

let add_primitive name prim_help prim =
  let prim_name = "OCaml_" ^ name in
  BuildOCamlVariables.ocamlmod_add name (VPrim prim_name);
  BuildOCP.add_primitive prim_name prim_help prim


let is_enabled options =
  BuildValue.get_bool_with_default options "enabled" true

let add_ocaml_package loc state config name kind =
  (*  Printf.eprintf "BuildOCamlPackage.add_ocaml_package %S\n%!" name; *)
  let (pk : unit BuildOCPTypes.package) =
    BuildOCP.define_package loc state config ~name ~kind
  in
  let env = config.config_env in
  let opk_version = BuildValue.get_string_with_default [env]
      "version"  "0.1-alpha" in
  let opk = {
    opk_name = name;
    opk_package = pk;
    opk_options = env;
    opk_dirname = pk.package_dirname;
    opk_kind = pk.package_type;
    opk_version;
    opk_id  = 0;
    opk_deps_map = StringMap.empty;
    opk_requires_map = IntMap.empty;
    opk_requires = [];
  } in
  pk.package_plugin <- OCamlPackage opk;
  if not ( is_enabled [env] ) then
    pk.package_disabled <- true;
  (*
  let s = BuildOCPPrinter.string_of_package (fun _ _ _ -> ()) pk in
  Printf.eprintf "New OCaml Description:\n";
  Printf.eprintf "%s\n%!" s;
  *)
  opk

let add_ocaml_package_unit loc state config name kind =
  let (_ : ocaml_description) =
    add_ocaml_package  loc state config name kind in
  ()

let add_ocaml_package_pk loc state config name kind =
  let opk = add_ocaml_package  loc state config name kind in
  opk.opk_package


let _ =
  (* For .ocp files, we can only define OCaml packages, so
     BuildOCP has a specific way to do it ! *)
  BuildOCP.add_ocaml_package := add_ocaml_package_pk;

  add_primitive "new_package"
    [ "Add a new OCaml package" ]
    (fun loc state config args ->
      match args with
      | [ VString name; VString kind; VObject config_env ] ->
        add_ocaml_package_unit loc state { config  with config_env }
          name (BuildOCP.package_type_of_string kind);
        BuildValue.unit
      |  _ ->
        BuildOCP2Prims.raise_bad_arity loc
          "OCaml.new_package(name,kind,ocaml)" 3 args
    );
  add_primitive "library"
    [ "Add a new OCaml library" ]
    (fun loc state config args ->
      match args with
      | [ VString name; VObject config_env ] ->
        add_ocaml_package_unit loc state { config  with config_env }
          name BuildOCPTypes.LibraryPackage;
        BuildValue.unit
      |  _ ->
        BuildOCP2Prims.raise_bad_arity loc
          "OCaml.library(name,ocaml)" 2 args
    );
  add_primitive "program"
    [ "Add a new OCaml program" ]
    (fun loc state config args ->
      match args with
      | [ VString name; VObject config_env ] ->
        add_ocaml_package_unit loc state { config  with config_env }
          name BuildOCPTypes.ProgramPackage;
        BuildValue.unit
      |  _ ->
        BuildOCP2Prims.raise_bad_arity loc
          "OCaml.library(name,ocaml)" 2 args
    )


(* This [getconf] primitive should probably be moved to BuildOCP,
as it is not specific to OCaml, isn't it ? *)
let () =
  BuildOCP.add_primitive "getconf"
    [ "getconf(name) returns the configuration associated with name" ]
    (fun loc state config args ->
      match args with
      | [VString name] ->
        begin
          try
            begin match BuildValue.config_get config "config" with
            | VObject env ->
              BuildValue.get [env] name
            | _ -> BuildValue.unit
            end
          with Var_not_found _ -> BuildValue.unit
        end
      | _ ->
        BuildOCP2Prims.raise_bad_arity loc
          "getconf(name)" 1 args
    )


type tpk = {
  tpk_id : int;
  tpk_name : string;
  tpk_pk : ocaml_description;
  tpk_names : string list;
  tpk_tags : string list;
  tpk_raw_requires : string package_dependency StringMap.t;
  mutable tpk_enabled : bool;
  mutable tpk_keys : (string * string) list;
  mutable tpk_requires : tpk IntMap.t;
  mutable tpk_required_by : tpk IntMap.t;
}

type package_comparison =
  PackageEquality
| PackageConflict
| PackageUpgrade of bool

let print_package_deps = ref false


let normalized_dir dir =
  File.to_string (File.of_string dir)


let new_dep pk pk2 options =
  try
    IntMap.find pk2.opk_id pk.opk_requires_map
  with Not_found ->
    let dep =
      {
        dep_project = pk2;
        dep_link = false;
        dep_syntax = false;
        dep_optional = false;
        dep_options = options;
      } in
    pk.opk_requires_map <-
      IntMap.add pk2.opk_id dep pk.opk_requires_map;
    pk.opk_requires <- dep :: pk.opk_requires;
    (*    Printf.eprintf "New dep %s <- %s\n%!"
          pk2.package_name pk.package_name; *)
    dep

let new_package_dep pk s env =
  try
    StringMap.find s pk.opk_deps_map
  with Not_found ->
    let dep = {
      dep_project = s;
      dep_link = false;
      dep_syntax = false;
      dep_optional = false;
      dep_options = env;
    }
    in
    pk.opk_deps_map <- StringMap.add s dep pk.opk_deps_map;
    dep

let add_project_dep pk s options =
  let dep = new_package_dep pk s options in
  dep.dep_link <- BuildValue.get_bool_with_default [options] "tolink" true;

  begin
    try
      dep.dep_optional <- BuildValue.get_bool [options] "optional"
    with Var_not_found _ -> ()
  end;
(*  Printf.eprintf "add_project_dep for %S = %S with link=%b\n%!"
    pk.package_name s dep.dep_link; *)
()


let print_deps msg pk =
  Printf.eprintf "%s: Project %s depends on:\n%!" msg pk.opk_name;
  List.iter (fun dep ->
    let pd = dep.dep_project in
    Printf.eprintf "\t%s %s%s%s%s\n%!"
      (BuildOCP.string_of_package_type pd.opk_kind)
      pd.opk_name
      (if dep.dep_link then "(link)" else "")
      (if dep.dep_syntax then "(syntax)" else "")
      (if dep.dep_optional then "(optional)" else "")
  ) pk.opk_requires

let update_deps pj =

  if !print_package_deps || verbose 5 then
    print_deps "BEFORE update_deps" pj;

  (*
    This computation depends on what we are dealing with:
    - For a syntax:
    - we must include all transitive link dependencies
    - for a Library dependency, we want to include all link dependencies
    - for a Syntax dependency, we want to also include syntax dependencies

    - For a library, all syntax dependencies should remain syntax
    dependencies. Link dependencies of syntaxes should become syntax
    dependencies.

    For now, we have three kinds of dependencies:
    1) 'link' dependencies: we must copy all 'link' transitive dependencies
    as new 'link' dependencies.
    2) 'syntax' dependencies: we must copy all 'link' transitive dependencies
    as new 'syntax' dependencies.
    3) neither 'link' nor 'syntax': we should not copy transitive dependencies.

    We cannot do it in one pass: we should first compute strong dependencies, and
    remove packages not meeting strong dependencies. Then, we can redo the
    computation, this time knowing which optional packages are available.

  *)

  List.iter (fun dep ->
    match dep.dep_project.opk_kind with
    | SyntaxPackage ->
      dep.dep_syntax <- true;
      dep.dep_link <- false;
    | RulesPackage
    | ProgramPackage ->
      dep.dep_syntax <- false;
      dep.dep_link <- false;
    | LibraryPackage
    | ObjectsPackage
      -> ()
    | TestPackage ->
      Printf.eprintf "Error: Test %S appears in requirements of %S\n%!"
        dep.dep_project.opk_name
        pj.opk_name;
      exit 2;
  ) pj.opk_requires;

  (* add all link dependencies, transitively *)
  let rec add_link_deps pj1 =
    List.iter (fun dep ->
      let pj2 = dep.dep_project in
      let dep2 = new_dep pj pj2 dep.dep_options in
      if verbose 5 then
        Printf.eprintf "%S -> %S\n" pj.opk_name pj2.opk_name;
      if dep.dep_link &&
        (not dep2.dep_link || pj1 == pj) then begin
          dep2.dep_link <- true;
          if verbose 5 then
            Printf.eprintf "%S -> link %S\n" pj.opk_name pj2.opk_name;
          add_link_deps pj2
        end
    ) pj1.opk_requires
  in
  add_link_deps pj;

  (* add syntax dependencies, and their link dependencies
     transformed into syntax dependencies *)
  let rec add_link_as_syntax_deps pj1 =
    List.iter (fun dep ->
      if dep.dep_link then
        let pj2 = dep.dep_project in
        let dep2 = new_dep pj pj2 dep.dep_options in
        if not dep2.dep_syntax then begin
          if verbose 5 then
            Printf.eprintf "%S -> syntax %S\n" pj.opk_name pj2.opk_name;
          dep2.dep_syntax <- true;
          add_link_as_syntax_deps pj2
        end
    ) pj1.opk_requires
  in

  let add_syntax_deps pj1 =
    List.iter (fun dep ->
      if dep.dep_syntax then
        let pj2 = dep.dep_project in
        let dep2 = new_dep pj pj2 dep.dep_options in
        if not dep2.dep_syntax || pj1 == pj then begin
          dep2.dep_syntax <- true;
          if verbose 5 then
            Printf.eprintf "%S -> syntax %S\n" pj.opk_name pj2.opk_name;
          add_link_as_syntax_deps pj2;
        end
    ) pj1.opk_requires
  in
  add_syntax_deps pj;
  if !print_package_deps ||  verbose 5 then
    print_deps "AFTER update_deps SORT" pj;
  ()

let verify_packages w state =

  let package_id = ref 0 in
  let packages = ref [] in

  IntMap.iter (fun i pk ->
    match pk.package_plugin with
    | OCamlPackage opk ->

      let options = opk.opk_options in

      if not opk.opk_package.package_disabled &&
        not ( BuildMisc.exists_as_directory opk.opk_dirname ) then begin
      (* TODO: we should probably do much more than that, i.e. disable also a
         package when some files are missing. *)
          BuildOCP.w_MissingDirectory w
            (
              opk.opk_dirname,
              opk.opk_name,
              pk.package_filename);
         pk.package_disabled <- true
        end else begin
          packages := opk :: !packages;
          opk.opk_id <- !package_id;
          incr package_id;


          List.iter (fun (s, options) ->
            add_project_dep opk s options
          ) (try BuildValue.prop_list (BuildValue.get [options] "requires")
            with Var_not_found _ ->
        (*    Printf.eprintf "No 'requires' for package %S\n%!" name; *)
              []
          )
        end

    | _ -> ()
  ) (BuildOCP.get_packages state);

  let packages = Array.of_list (List.rev !packages) in
  (*
  let packages = final_state state in

  (* Verify that package directories really exist, and add 'requires'
     to pk.package_deps_map *)
  Array.iter (check_package w) packages;
  *)

  let disabled_packages = ref [] in
  let tpk_packages = ref [] in
  let tpk_id = ref 0 in

  (* (1) Verify that a given package is only provided by one
     package, discarding older versions and so on. *)
  let h = Hashtbl.create 113 in

  let remove_tpk tpk =
    List.iter (Hashtbl.remove h) tpk.tpk_keys;
    tpk.tpk_keys <- [];
    tpk.tpk_enabled <- false;
  in

  let compare_tpks tpk1 tpk2 =
    let pk1 = tpk1.tpk_pk in
    let pk2 = tpk2.tpk_pk in
    let o1 = pk1.opk_options in
    let o2 = pk2.opk_options in
    let pk1_generated = BuildValue.get_bool_with_default [o1] "generated" false in
    let pk2_generated = BuildValue.get_bool_with_default [o2] "generated" false in
    match pk1_generated, pk2_generated with
      false, false ->
        PackageConflict
    | true, true ->
      if normalized_dir pk1.opk_dirname =
        normalized_dir pk2.opk_dirname &&
          pk1.opk_kind = pk2.opk_kind &&
        pk1.opk_version = pk2.opk_version
      (* TODO: We should also test for asm/byte... *)
      then begin
        if verbose 5 then
          Printf.eprintf "Discarding duplicated package %s_%d\n%!"
            pk1.opk_name pk1.opk_package.package_id;
        PackageEquality
      end
      else
        begin
          if verbose 5 then begin
            Printf.eprintf "Conflict over %S\n" pk1.opk_name;
            Printf.eprintf "dirname: %S\n" pk1.opk_dirname;
            Printf.eprintf "type: %S\n"
              (BuildOCP.string_of_package_type pk1.opk_kind);
            Printf.eprintf "version: %S\n" pk1.opk_version;
            Printf.eprintf "dirname: %S\n" pk2.opk_dirname;
            Printf.eprintf "type: %S\n"
              (BuildOCP.string_of_package_type pk2.opk_kind);
            Printf.eprintf "version: %S\n" pk2.opk_version;
          end;
          PackageConflict
        end

    (* TODO: if we have the sources of a new version, we should probably
       accept the new version, but not accept any installed version
       depending on the old version.

       TODO: implement this in a different pass : we should filter out
       packages that are duplicated, especially to discard installed
       packages. But what if we have the sources of A, { A + B }
       installed, and not the sources of B ?

       In the meantime, the solution is to desinstall the installed
       versions when they conflict.  *)

    | _, tpk2_installed -> PackageUpgrade tpk2_installed
  in
  (* A package with a given tag can only be provided by one package.
     This will change in the future, but cannot be handled currently. *)

  let verify_unicity name tag_name tpk =
    let key =  (name, tag_name) in
    try
      let tpk2 = Hashtbl.find h key in
      match compare_tpks tpk tpk2 with

      | PackageEquality ->
        (* TODO: we should keep the best *)
        remove_tpk tpk2;
        raise Not_found

      | PackageConflict ->
          BuildOCP.w_PackageConflict w (tpk.tpk_pk.opk_package,
                             tpk2.tpk_pk.opk_package, tpk.tpk_pk.opk_package);
        remove_tpk tpk2;
        raise Not_found

      (* Here is a bug:
       * We have installed A and B, with B using A
       * We now want to install a new version of A
       *)

      | PackageUpgrade add_new ->
        (*        Printf.eprintf "Upgrading %S\n%!" name; *)
        if add_new then begin
          remove_tpk tpk2;
          raise Not_found
        end else begin
          remove_tpk tpk;
          raise Exit
        end

    with Not_found ->
      Hashtbl.add h key tpk;
      tpk.tpk_keys <- key :: tpk.tpk_keys
  in


  let check_package_unicity opk =
    if verbose 5 || !print_package_deps then
      Printf.eprintf "check_package_unicity %s_%d of %s\n%!"
        opk.opk_name
        opk.opk_package.package_id
        opk.opk_dirname;
    (*
      StringMap.iter (fun name pkdep ->
      Printf.eprintf "\t%s depends on %s\n%!"
      opk.opk_name pkdep.dep_project
      ) opk.package_deps_map;
    *)
    let envs = [opk.opk_options] in
    if not opk.opk_package.package_disabled then
      try
        let tpk_name = opk.opk_name in
        let tpk_tags =
          match opk.opk_kind with
          | ProgramPackage
          | LibraryPackage
          | ObjectsPackage
          | SyntaxPackage -> begin
            match
              BuildValue.get_bool_with_default envs "has_byte" true,
              BuildValue.get_bool_with_default envs "has_asm" true with
                true, true
              | false, false (* both disabled ? probably a mistake *)
                -> [ "byte"; "native" ]
              | true, false -> [ "byte" ]
              | false, true -> [ "native" ]
          end
          | RulesPackage -> []
          | TestPackage -> []
        in
        let tpk_names = BuildValue.get_strings_with_default envs "provides" [] in
        incr tpk_id;
        let tpk = {
          tpk_id = !tpk_id;
          tpk_requires = IntMap.empty; tpk_required_by = IntMap.empty;
          tpk_raw_requires = StringMap.empty;
          tpk_pk = opk; tpk_enabled = true; tpk_keys = [];
          tpk_name; tpk_tags; tpk_names } in
        tpk_packages := tpk :: !tpk_packages;
        verify_unicity tpk_name "" tpk;
        List.iter (fun tag_name ->
          List.iter (fun package_name ->
            verify_unicity package_name tag_name tpk
          ) (tpk_name :: tpk_names)
        ) tpk_tags
      with Exit ->
        (* Happens if verify_unicity discards this package because
           it knows a better one. *)
        ()
(*    else
      disabled_packages := pk :: !disabled_packages *)
  in
  Array.iter check_package_unicity packages;



  let superseded_packages = ref [] in
  let enabled_packages = ref [] in
  List.iter (fun tpk ->
    if tpk.tpk_enabled then
      enabled_packages := tpk :: !enabled_packages
    else
      superseded_packages := tpk.tpk_pk :: !superseded_packages
  ) !tpk_packages;

  (* 2. Ok, we have only one package providing every key, so we can start
     associating dependencies. However, there are also two cases:
     - some dependencies are not at all provided. This can lead to either
     completely disabling a package, or just removing either the "byte"
     or "native" part.

     If we depend on either a program or a syntax, it _always_ means that
     we depend on the bytecode part, unless the program/syntax is only
     provided in bytecode.
  *)

  let ready_queue = ref [] in
  let waiting_queue = ref IntMap.empty in

  let add_requires tpk tpk2 =
    tpk.tpk_requires <- IntMap.add tpk2.tpk_id tpk2 tpk.tpk_requires;
    tpk2.tpk_required_by <- IntMap.add tpk.tpk_id tpk tpk2.tpk_required_by
  in

  let add_requires tpk tag_name =
    let pk = tpk.tpk_pk in
    StringMap.iter (fun _ dep ->
      try
        let tpk2 = Hashtbl.find h (dep.dep_project, tag_name) in
        add_requires tpk tpk2
      with Not_found ->
        (*        Printf.eprintf "Warning: missing dependency, %S requires %S/%s\n%!"
                  tpk.tpk_name dep.dep_project tag_name; *)
        ()
    ) pk.opk_deps_map
  in
  List.iter (fun tpk ->
    let pk = tpk.tpk_pk in
    match pk.opk_kind with
    | RulesPackage
    | TestPackage ->
      add_requires tpk ""

    | ProgramPackage
    | LibraryPackage
    | ObjectsPackage
    | SyntaxPackage ->

      let envs = [pk.opk_options] in
      add_requires tpk "";
      let tag_names =
        match
          BuildValue.get_bool_with_default envs "has_byte" true,
          BuildValue.get_bool_with_default envs "has_asm" true with
            true, true | false, false -> [ "byte"; "native"]
          | true, false -> [ "byte" ]
          | false, true -> [ "native" ]
      in
      List.iter (add_requires tpk) tag_names
  ) !enabled_packages;

  List.iter (fun tpk ->
    if verbose 5 || !print_package_deps then

      Printf.eprintf "Taking %S from enabled_packages\n%!" tpk.tpk_pk.opk_name;
    if IntMap.cardinal tpk.tpk_requires = 0 then
      ready_queue := tpk :: !ready_queue
    else
      waiting_queue := IntMap.add tpk.tpk_id tpk !waiting_queue
  ) !enabled_packages;

  let missing_dep_of = ref StringMap.empty in
  let missing_dep_on = ref StringMap.empty in

  (*  let missing_packages = ref [] in *)
  (*  let incomplete_packages = ref [] in *)
  let sorted_packages = ref [] in
  let h2 = Hashtbl.create 113 in
  let add_require pk (dep, pk2) =
    (*    Printf.eprintf "add_require %s_%d on %s_%d\n%!"
          pk.opk_name pk.package_id
          pk2.opk_name pk2.package_id; *)
    if (BuildValue.get_bool_with_default [pk.opk_options] "generated" false)
      && not (BuildValue.get_bool_with_default [pk2.opk_options] "generated" false)
    then begin
      match pk.opk_kind with
      | ProgramPackage
      | RulesPackage
      | SyntaxPackage -> ()
      | LibraryPackage
      | TestPackage
      | ObjectsPackage ->
        (*
        Printf.eprintf
          "Warning: installed package %s depends on source package %s\n%!"
          pk.opk_name pk2.opk_name;
        *)
        BuildOCP.w_BadInstalledPackage w (pk.opk_name, pk2.opk_name);
        raise Exit
    end;

    let dep2 = new_dep pk pk2 dep.dep_options in
    dep2.dep_link <- dep.dep_link;
    dep2.dep_syntax <- dep.dep_syntax;
  in
  let add_missing pk tag dep =
    BuildOCP.w_MissingPackage w (dep.dep_project, [pk.opk_package]);
    let dep = dep.dep_project ^ ":" ^ tag in
    let pk = pk.opk_name ^ ":" ^ tag in
    begin
      try
        let r = StringMap.find dep !missing_dep_of in r := pk :: !r
      with Not_found ->
        missing_dep_of := StringMap.add dep (ref [pk]) !missing_dep_of
    end;
    begin
      try
        let r = StringMap.find pk !missing_dep_on in r := dep :: !r
      with Not_found ->
        missing_dep_on := StringMap.add pk (ref [dep]) !missing_dep_on
    end
  in
  while !ready_queue <> [] do
    let queue = !ready_queue in
    ready_queue := [];
    List.iter (fun tpk ->
      if verbose 5 || !print_package_deps then
        Printf.eprintf "Taking %S from ready_queue\n%!" tpk.tpk_pk.opk_name;
      IntMap.iter (fun _ tpk2 ->
        tpk2.tpk_requires <- IntMap.remove tpk.tpk_id tpk2.tpk_requires;
        if IntMap.cardinal tpk2.tpk_requires = 0 then begin
          if verbose 5 || !print_package_deps then
            Printf.eprintf "Adding %S to ready_queue!\n%!" tpk2.tpk_pk.opk_name;
          ready_queue := tpk2 :: !ready_queue;
          waiting_queue := IntMap.remove tpk2.tpk_id !waiting_queue
        end
      ) tpk.tpk_required_by;
      let pk = tpk.tpk_pk in
      if verbose 5 || !print_package_deps then
        Printf.eprintf "Examining package %S\n" pk.opk_name;
      let envs = [pk.opk_options] in

      try
        match pk.opk_kind with
        | RulesPackage ->

          StringMap.iter (fun _ dep ->
            try
              let pk2 = Hashtbl.find h2 (dep.dep_project, "") in
              add_require pk (dep, pk2)
            with Not_found ->
              BuildOCP.w_MissingDependency w
                ("", tpk.tpk_name, dep.dep_project);
(*
              Printf.eprintf "Warning: missing dependency, %S requires %S\n%!"
                tpk.tpk_name dep.dep_project;
*)
              add_missing pk "" dep;
              raise Exit
          ) pk.opk_deps_map;
          Hashtbl.add h2 (pk.opk_name, "") pk;
          List.iter (fun package_name ->
            Hashtbl.add h2 (package_name, "") pk;
          ) (BuildValue.get_strings_with_default envs "provides" []);
          sorted_packages := pk :: !sorted_packages

        | TestPackage
        | ProgramPackage
        | LibraryPackage
        | ObjectsPackage
        | SyntaxPackage ->

          let envs = [pk.opk_options] in
          let had_byte = BuildValue.get_bool_with_default envs "has_byte" true in
          let had_asm = BuildValue.get_bool_with_default envs "has_asm" true in
          let has_byte = ref had_byte in
          let has_asm = ref had_asm in

          let byte_requires = ref [] in
          let asm_requires = ref [] in
          pk.opk_requires <- [];



          (* bytecode version *)
          if !has_byte then begin
            try
              StringMap.iter (fun _ dep ->
                try
                  let pk2 = Hashtbl.find h2 (dep.dep_project, "byte") in
                  match pk2.opk_kind with
                  | RulesPackage -> assert false
                  | TestPackage (* TODO: why would we depend on a test package ?? *)
                  | LibraryPackage
                  | ProgramPackage
                  | ObjectsPackage
                  | SyntaxPackage
                    ->
                    byte_requires := (dep, pk2) :: !byte_requires
                with Not_found ->
                  try
                    let pk2 = Hashtbl.find h2 (dep.dep_project, "native") in
                    match pk2.opk_kind with
                    | RulesPackage -> assert false
                    | TestPackage ->
                                            (* TODO: why would we depend on a test package ?? *)
                      byte_requires := (dep, pk2) :: !byte_requires;
                    | LibraryPackage
                    | ObjectsPackage
                      ->
                      BuildOCP.w_KindMismatch w
                        ("bytecode", pk.opk_name, "native", pk2.opk_name);
                        raise Exit
                    | ProgramPackage
                    | SyntaxPackage ->
                      byte_requires := (dep, pk2) :: !byte_requires

                  with Not_found ->
                    try
                      let pk2 = Hashtbl.find h2 (dep.dep_project, "") in
                      match pk2.opk_kind with
                      | RulesPackage ->
                        byte_requires := (dep, pk2) :: !byte_requires;
                      | TestPackage ->
                                              (* TODO: why would we depend on a test package ?? *)
                        byte_requires := (dep, pk2) :: !byte_requires;
                      | LibraryPackage
                      | ProgramPackage
                      | ObjectsPackage
                      | SyntaxPackage
                        -> assert false
                    with Not_found ->
                      BuildOCP.w_MissingDependency w
                        ("bytecode", tpk.tpk_name, dep.dep_project);
(*
                      Printf.eprintf "Warning: missing dependency, bytecode %S requires %S bytecode\n%!"
                        tpk.tpk_name dep.dep_project;
*)
                      add_missing pk "byte" dep;
                      raise Exit
              ) pk.opk_deps_map;
            with Exit ->
              (*              Printf.eprintf "\tDisabling bytecode version of %S\n%!" tpk.tpk_name; *)
              has_byte := false;
          end;




          (* native version *)
          if !has_asm then begin
            try
              StringMap.iter (fun _ dep ->
                try
                  let pk2 = Hashtbl.find h2 (dep.dep_project, "native") in
                  match pk2.opk_kind with
                  | RulesPackage -> assert false
                  | TestPackage (* TODO: why would we depend on a test package ?? *)
                  | LibraryPackage
                  | ProgramPackage
                  | ObjectsPackage
                  | SyntaxPackage
                    ->
                    asm_requires := (dep, pk2) :: !asm_requires
                with Not_found ->
                  try
                    let pk2 = Hashtbl.find h2 (dep.dep_project, "byte") in
                    match pk2.opk_kind with
                    | RulesPackage -> assert false
                    | TestPackage ->
                                           (* TODO: why would we depend on a test package ?? *)
                      asm_requires := (dep, pk2) :: !asm_requires;
                    | LibraryPackage
                    | ObjectsPackage
                      ->
                      BuildOCP.w_KindMismatch w ("native", pk.opk_name,
                                                 "bytecode", pk2.opk_name);
                        raise Exit
                    | ProgramPackage
                    | SyntaxPackage ->
                      asm_requires := (dep, pk2) :: !asm_requires

                  with Not_found ->
                    try
                      let pk2 = Hashtbl.find h2 (dep.dep_project, "") in
                      match pk2.opk_kind with
                      | RulesPackage ->
                        asm_requires := (dep, pk2) :: !asm_requires;
                      | TestPackage ->
                                             (* TODO: why would we depend on a test package ?? *)
                        asm_requires := (dep, pk2) :: !asm_requires;
                      | LibraryPackage
                      | ProgramPackage
                      | ObjectsPackage
                      | SyntaxPackage
                        -> assert false
                    with Not_found ->
                      BuildOCP.w_MissingDependency w
                        ("native", tpk.tpk_name, dep.dep_project);

                      (*                Printf.eprintf "Warning: missing dependency, native %S requires %S native\n%!"
                                                             tpk.tpk_name dep.dep_project; *)
                      add_missing pk "asm" dep;
                      raise Exit
              ) pk.opk_deps_map;
            with Exit ->
              (*              Printf.eprintf "\tDisabling native version of %S\n%!" tpk.tpk_name; *)
              has_asm := false;
          end;

          if !has_byte || !has_asm then begin
            List.iter (add_require pk) !byte_requires;
            List.iter (add_require pk) !asm_requires;
            sorted_packages := pk :: !sorted_packages;
            Hashtbl.add h2 (pk.opk_name, "") pk;
            if !has_byte then begin
              Hashtbl.add h2 (pk.opk_name, "byte") pk;
              List.iter (fun package_name ->
                Hashtbl.add h2 (package_name, "byte") pk;
              ) (BuildValue.get_strings_with_default envs "provides" []);
            end else
              if had_byte then
                pk.opk_options <- BuildValue.set_bool pk.opk_options
                  "has_byte" false;
            if !has_asm then begin
              Hashtbl.add h2 (pk.opk_name, "native") pk;
              List.iter (fun package_name ->
                Hashtbl.add h2 (package_name, "native") pk;
              ) (BuildValue.get_strings_with_default envs "provides" []);
            end else
              if had_asm then
                pk.opk_options <- BuildValue.set_bool pk.opk_options
                  "has_asm" false;
          end else begin
            raise Exit
          end
      with Exit ->
        tpk.tpk_enabled <- false;
        disabled_packages := pk :: !disabled_packages;
        BuildOCP.w_IncompletePackage w pk.opk_package
    ) queue
  done;

  (*
  if !missing_dep_of <> StringMap.empty then begin
    let ncount = ref 0 in
    StringMap.iter (fun dep _pkgs ->
      if not (StringMap.mem dep !missing_dep_on) then
        incr ncount
    ) !missing_dep_of;
    if !print_missing_deps then begin
      Printf.eprintf "Warning: %d missing dependencies\n" !ncount;
      StringMap.iter (fun dep _pkgs ->
        if not (StringMap.mem dep !missing_dep_on) then
          let rec iter indent dep =
            try
              let pkgs = StringMap.find dep !missing_dep_of in
              List.iter (fun pk ->
                Printf.eprintf "%s" indent;
                Printf.eprintf "<- %S\n" pk;
                iter (indent ^ "     ") pk;
              ) !pkgs
            with Not_found -> ()
          in
          Printf.eprintf "   %S\n" dep;
          iter           "      " dep;
      ) !missing_dep_of;
      Printf.eprintf "Warning: %d disabled packages for missing deps\n"
        (List.length !incomplete_packages);
      List.iter (fun pk ->
        Printf.eprintf "  %S (%s)\n" pk.opk_name pk.opk_dirname;
      ) !incomplete_packages
    end else begin
      Printf.eprintf "Warning: %d missing and %d disabled (use -print-missing)\n%!" !ncount (List.length !incomplete_packages);
    end;
 end;
  *)

  if IntMap.cardinal !waiting_queue > 0 then begin
    Printf.eprintf "*****************************************************\n";
    Printf.eprintf "*****************************************************\n";
    Printf.eprintf "*****************************************************\n";
    Printf.eprintf "*****************************************************\n";
    Printf.eprintf "*****************************************************\n";
    Printf.eprintf "%d packages waiting in waiting queue !\n%!"
      (IntMap.cardinal !waiting_queue);
    IntMap.iter (fun _ tpk ->
      Printf.eprintf "  %S still requires:\n%!" tpk.tpk_pk.opk_name;
      IntMap.iter (fun _ tpk ->
        Printf.eprintf "     %S\n%!" tpk.tpk_pk.opk_name;
      ) tpk.tpk_requires
    ) !waiting_queue;
  end;

  let sorted_packages = List.rev !sorted_packages in

  (*
    (* N. We have to check that installed packages only depend on
    installed packages ! *)
    List.iter (fun pk ->
    if get_bool_with_default [pk.opk_options] "generated" false then
    List.iter (fun dep2 ->
    let pk2 = dep2.dep_project in
    if not ( get_bool_with_default [pk2.opk_options] "generated" false )
    then begin
    Printf.eprintf "ERROR ERROR !!!\n%!";
    Printf.eprintf "Installed %s depends on non installed %s\n%!"
    pk.opk_name pk2.opk_name;
    exit 2
    end;
    ) pk.package_requires

    ) sorted_packages;
  *)

  List.iter update_deps sorted_packages;


(*

  (*  let npackages = Array.length packages in *)

  let pj = {
    project_sorted = Array.of_list sorted_packages;
    project_disabled = Array.of_list !disabled_packages;
    (*
    project_missing = !missing_packages;
    project_incomplete = Array.of_list !incomplete_packages;
    *)
  } in
  (* TODO: fix this assertion. The equality stands only if we count
     also duplicated packages.
  assert (npackages >=
            Array.length pj.project_sorted +
            Array.length pj.project_incomplete +
            Array.length pj.project_disabled);
  *)

  (* Change the package IDs: the package_requires_map is not correct anymore ! *)
  reset_package_ids "project_sorted" pj.project_sorted;

  (* TODO: The impact of this is that all dependencies are sorted in
     the same order in all packages. This might, however, not be what
     someone wants, because you might want to have a different link
     order than the one globally inferred.  *)
  Array.iter (fun pk ->
    if requires_keep_order_option.get [pk.opk_options]  then begin
      (* This option does not work.

      Printf.eprintf "pk=%s\n%!" pk.opk_name;
      List.iter (fun dep ->
        Printf.eprintf "  pj=%s\n%!" dep.dep_project.opk_name;
        IntMap.iter (fun _ dep ->
          Printf.eprintf "    pj=%s\n%!" dep.dep_project.opk_name
        ) dep.dep_project.pi.package_requires_map;
        List.iter (fun name ->
          Printf.eprintf "     after: %s\n%!" name;
          try
            let pj2 = StringMap.find name pk.pi.package_deps_map in
            Printf.eprintf "pj2 = %s\n%!"
              pj2.dep_project.opk_name;
            force_add_dep dep pj2
          with Not_found ->
            Printf.eprintf "Project %s not found\n%!" name
        ) (BuildValue.get_strings_with_default
            [dep.dep_options]  "after"  []);
      ) pk.pi.package_requires;
      *)
      let (sorted, cycle, _ ) = PackageLinkSorter.sort pk.pi.package_requires in
      assert (cycle = []);
      (*
      Printf.eprintf "pk=%s\n%!" pk.opk_name;
      List.iter (fun dep ->
        Printf.eprintf "  pj=%s\n%!" dep.dep_project.opk_name;
      ) sorted;
      *)
      pk.pi.package_requires <- sorted
    end else begin
      pk.pi.package_requires <- List.sort (fun dep1 dep2 ->
        compare
          dep1.dep_project.package_id
          dep2.dep_project.package_id) pk.pi.package_requires;
    end;

    if !print_package_deps || verbose 9 then begin
      Printf.eprintf "Package %S[%d]\n" pk.opk_name pk.package_id;
      List.iter (fun dp ->
        Printf.eprintf "\t%S[%d]%s%s\n"
          dp.dep_project.opk_name
          dp.dep_project.package_id
          (if dp.dep_link then " (link)" else "")
          (if dp.dep_syntax then " (syntax)" else "");
      ) pk.pi.package_requires
    end;
  ) pj.project_sorted;

  (* Do this after toposort ! *)
  Array.iter (fun pk ->
    pk.pi.package_requires_map <- IntMap.empty
  ) pj.project_sorted;

  (*  reset_package_ids "project_incomplete" pj.project_incomplete; *)
  reset_package_ids "project_disabled" pj.project_disabled;
  (*
    begin match !print_dot_packages with
    None -> ()
    | Some filename ->
    dump_dot_packages filename pj
    end;
  *)

  pj
*)

  List.iter (fun opk ->
    let pk = opk.opk_package in
    pk.package_disabled <- true;
  ) !disabled_packages;

  List.iter (fun opk ->
    let pk = opk.opk_package in
    pk.package_disabled <- true;
  ) !superseded_packages;

  let package_id = ref 0 in
  List.iter (fun opk ->
    let pk = opk.opk_package in
    pk.package_requires_list <- [];
    IntMap.iter (fun _ dep ->
      pk.package_requires_list <- dep.dep_project.opk_package
        :: pk.package_requires_list
    ) opk.opk_requires_map;
    opk.opk_id <- !package_id;
    incr package_id;
  ) sorted_packages;

  List.iter (fun opk ->
    opk.opk_requires <- List.sort (fun dep1 dep2 ->
      compare dep1.dep_project.opk_id dep2.dep_project.opk_id)
      opk.opk_requires;
    opk.opk_requires_map <- IntMap.empty;
    opk.opk_deps_map <- StringMap.empty;
  ) sorted_packages;

()

let pk_opk pk =
  match pk.package_plugin with
  | OCamlPackage opk -> opk
  | _ -> assert false

let init_env env_pj =

  BuildOCamlVariables.packages_option.set
    (VList (Array.to_list (Array.map (fun pk ->
      let opk = pk_opk pk in
      let dirname = BuildGlobals.absolute_filename pk.package_dirname in
      List.iter (fun suffix ->
        BuildSubst.add_to_global_subst (pk.package_name ^ suffix) dirname)
        [ "_SRC_DIR"; "_DST_DIR"; "_FULL_SRC_DIR"; "_FULL_DST_DIR" ];
      VTuple [VString pk.package_name; VObject opk.opk_options]
     ) env_pj.project_sorted)));
  ()



let () =
  Printf.eprintf "OCamlPlugin: enabled.\n%!";
  BuildOCP.plugin_verifiers := verify_packages :: !BuildOCP.plugin_verifiers;
  ()

let init () = ()
