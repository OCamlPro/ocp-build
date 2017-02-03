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

let continue_on_ocp_error = ref false

type state = {
  mutable packages : pre_package IntMap.t;
  mutable npackages : int;
  mutable config_files : Digest.t StringMap.t;
}



let print_conflict pk pk2 pk3 =
  let b = Buffer.create 100 in
  Printf.bprintf b "Warning: two projects called %S\n" pk.package_name;
  let print_package msg pk =
    let msg_len = String.length msg in
    let dirname_len = String.length pk.package_dirname in
    let filename_len = String.length pk.package_filename in
    if msg_len + dirname_len + filename_len > 70 then
      Printf.bprintf b "  %s %s\n     (%s)\n" msg
        pk.package_dirname pk.package_filename
    else
      Printf.bprintf b "  %s %s (%s)\n" msg
        pk.package_dirname pk.package_filename;
  in
  print_package "In" pk;
  print_package "In" pk2;
  print_package "Keeping" pk3;
  Buffer.contents b

let w_MissingDirectory w (dirname, name, filename) =
  BuildWarnings.wprintf w
    "Warning: directory %S for package does not exist: \
            \   Package %S in %S disabled.\n%!"
      dirname name filename
let w_PackageConflict w (pk1, pk2, pk3) =
  BuildWarnings.add w (print_conflict pk1 pk2 pk3)
let w_BadInstalledPackage w (name1, name2) =
  BuildWarnings.wprintf w
    "Warning: installed package %s depends on source package %s\n%!"
    name1 name2
let w_MissingDependency w (kind, name, dep) =
  BuildWarnings.wprintf w
    "Warning: missing dependency, %s %S requires %S\n%!"
    kind name dep
let w_KindMismatch w (kind, name, kind2, name2) =
  BuildWarnings.wprintf w
    "Warning: %s %S depends on %S, that only exists in %s\n%!"
      kind name name2 kind2
let w_IncompletePackage w pk =
  BuildWarnings.wprintf w
    "Warning: package %S disabled\n" pk.package_name
let w_MissingPackage w (name, pks) =
  BuildWarnings.wprintf w
    "Warning: missing package %S, needed by\n  %s\n" name
    (String.concat "  \n"
       (List.map (fun pk -> String.escaped pk.package_name) pks))

let initial_state () =
{ packages = IntMap.empty; npackages = 0; config_files = StringMap.empty; }

let copy_state s =
  { s with packages = s.packages }

    (*
let new_package_info () =
{
    package_node = LinearToposort.new_node ();
    package_validated = false;
    package_deps_map = StringMap.empty;
    package_requires = [];
    package_requires_map = IntMap.empty;
    package_added = false;
}
    *)

let final_state state =
  if state.npackages = 0 then [||] else
    Array.init state.npackages (fun i ->
      { (IntMap.find i state.packages) with
        (*        pi = new_package_info (); *)
        package_requires_list = [];
      }
    )

let get_packages state = state.packages

let new_package package_loc state name dirname filename filenames kind options =
  let package_id = state.npackages in
    (* Printf.eprintf "new_package %s_%d\n%!" name package_id; *)
  state.npackages <- state.npackages + 1;
  let pk = {
    package_source_kind = "ocp";
    package_id = package_id;
    (*    package_auto = None; *)
    (*    package_version = ""; *)
    package_loc;
    package_filename = filename;
    package_filenames = filenames;
    package_name = name;
    package_provides = name;
    package_type = kind;
    package_dirname = dirname;
    (*    package_options = options; *)
    package_plugin = Not_found;
    package_disabled = false;
    package_requires_list = [];
    (*    package_pk = pk; *)
    package_node = LinearToposort.new_node ();
  } in
  state.packages <- IntMap.add pk.package_id pk state.packages;
  (*
  let s = BuildOCPPrinter.string_of_package (fun _ _ _ -> ()) pk in
  Printf.eprintf "New package:\n";
  Printf.eprintf "%s\n%!" s;
  *)
  pk

let empty_config () = BuildValue.empty_config ()
let generated_config () =
  let empty_config = empty_config () in
  {
    empty_config with
      config_env = BuildValue.set_bool empty_config.config_env "generated" true;
  }

  (*
let new_package_dep pk s env =
    try
      StringMap.find s pk.pi.package_deps_map
    with Not_found ->
      let dep = {
        dep_project = s;
        dep_link = false;
        dep_syntax = false;
        dep_optional = false;
        dep_options = env;
      }
      in
      pk.pi.package_deps_map <- StringMap.add s dep pk.pi.package_deps_map;
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
  *)

(* We want to check the existence of the dirname of a package as soon
 as possible, so that we can disable it and enable another one.
 Actually, this should only be done for .ocpi files, i.e. installed files,
 for which we should use another loading phase.
*)

  (*
let check_package w pk =
  let options = pk.package_options in

    if BuildValue.get_bool_with_default [options] "enabled" true &&
       not ( BuildMisc.exists_as_directory pk.package_dirname ) then begin
      (* TODO: we should probably do much more than that, i.e. disable also a
         package when some files are missing. *)
         BuildWarnings.add w
           (MissingDirectory
               (
                 pk.package_dirname,
                 pk.package_name,
                 pk.package_filename));
         pk.package_options <- BuildValue.set_bool options "enabled" false;
    end else begin

      pk.package_version <- BuildValue.get_string_with_default [pk.package_options]
          "version"  "0.1-alpha";
      List.iter (fun (s, options) ->
        add_project_dep pk s options
      ) (try BuildValue.prop_list (BuildValue.get [pk.package_options] "requires")
        with Var_not_found _ ->
        (*    Printf.eprintf "No 'requires' for package %S\n%!" name; *)
        []
      )
    end
  *)

let define_package loc pj config
    ~name
    ~kind
    =
  (*  Printf.eprintf "define_package: substitute dirname\n%!"; *)
  let dirname =
    try
      let list = BuildValue.get_strings [config.config_env] "dirname"  in
      (* Printf.eprintf "subst_global...\n%!"; *)
      BuildSubst.subst_global (String.concat Filename.dir_sep list)
    with Var_not_found _ ->
      config.config_dirname
  in
  (* Printf.eprintf "dirname = %S\n%!" dirname;*)
  let dirname = if dirname = "" then "." else dirname in
  new_package loc pj name
    dirname
    config.config_filename config.config_filenames kind config.config_env

let subst_basename filename =
  let basename = Filename.basename filename in
  try
    let pos = String.index basename '.' in
    String.sub basename 0 pos
  with Not_found -> basename

let filesubst = BuildSubst.create_substituter
    [
      "file", (fun (file, (_env : env list) ) -> file);
      "basefile", (fun (file, _env) -> Filename.basename file);
      "basename", (fun (file, _env) -> subst_basename file);
      "dirname", (fun (file, _env) -> Filename.dirname file);
      "extensions", (fun (file, _env) ->
        try
          let pos = String.index file '.' in
          String.sub file pos (String.length file - pos)
        with Not_found -> "");
    ]

let string_of_package_type kind =
  match kind with
    ProgramPackage -> "program"
  | LibraryPackage -> "library"
  | SyntaxPackage -> "syntax"
          (*	  | ProjectToplevel -> "toplevel" *)
  | ObjectsPackage -> "objects"
  | TestPackage -> "test"
  | RulesPackage -> "rules"

let package_type_of_string kind =
  match kind with
    "program" -> ProgramPackage
  | "library" -> LibraryPackage
  | "syntax" -> SyntaxPackage
          (*	  | ProjectToplevel -> "toplevel" *)
  | "objects" -> ObjectsPackage
  | "test" -> TestPackage
  | "rules" -> RulesPackage
  | _ ->
    Printf.eprintf "Error: inexistent kind %S for package\n%!" kind;
    assert false

let add_ocaml_package =
  ref (fun _loc _state _config _name _kind -> assert false)

module OCP_arg = struct

    type context = state

    let filesubst = filesubst

    let parse_error () =
      if not !continue_on_ocp_error then exit 2

    let define_package loc ctx config ~name ~kind =
      let ( _ : unit package) =
        !add_ocaml_package loc ctx config name (package_type_of_string kind)
      in
      ()

    let new_file ctx filename digest =
      try
        let digest2 = StringMap.find filename ctx.config_files in
        if digest <> digest2 then begin
          Printf.eprintf "File %S modified during built. Exiting.\n%!" filename;
          exit 2
        end
      with Not_found ->
        ctx.config_files <-
          StringMap.add filename digest ctx.config_files

  end

module EvalOCP1 = BuildOCPInterp.Eval(OCP_arg)
module EvalOCP2 = BuildOCP2Interp.Eval(OCP_arg)

let add_primitive = EvalOCP2.add_primitive
let primitives_help = EvalOCP1.primitives_help

let verbose = DebugVerbosity.verbose ["B";"BP"] "BuildOCP"

let print_missing_deps = ref false

let init_packages () =
  let packages = initial_state () in
  packages

let print_loaded_ocp_files = ref false
(* let print_dot_packages = ref (Some "_obuild/packages.dot") *)
let print_package_deps = ref false

let load_ocp_files config packages files =

  List.iter (fun file ->
    let file = File.to_string file in
    let basename = Filename.basename file in
    if Filename.check_suffix file ".ocp2" &&
      String.lowercase basename <> "build.ocp2" then begin
        if verbose 5 || !print_loaded_ocp_files then
          Printf.eprintf "Reading module %s\n%!" file;
        let (_config : config) = EvalOCP2.read_ocamlconf file packages config in
        let modname = String.capitalize
          (Filename.chop_suffix basename ".ocp2") in
        if not (StringMap.mem modname !(config.config_modules)) then begin
          Printf.eprintf "Error: file %S did not define module %S\n%!"
            file modname;
          exit 2
          end
      end
  ) files;

  let nerrors = ref 0 in
  let rec iter parents files =
    match files with
      [] -> ()
    | file :: next_files ->
      match parents with
        [] -> assert false
      | (parent, filename, config) :: next_parents ->
        let file = File.to_string file in
        if OcpString.starts_with file parent then
          let dirname = Filename.dirname file in
          if verbose 5 || !print_loaded_ocp_files then
            Printf.eprintf "Reading %s with context from %s\n%!" file filename;
          let config =
            try
              if Filename.check_suffix file ".ocp" then
                EvalOCP1.read_ocamlconf file packages config
              else
                if Filename.basename file = "build.ocp2" then
                  EvalOCP2.read_ocamlconf file packages config
                else
                  config
            with BuildMisc.ParseError ->
              incr nerrors;
              config
          in
          iter ( (dirname, file, config) :: parents ) next_files
        else
          iter next_parents files
  in
  iter [ "", "<root>", config ] files;
  !nerrors

(*
  let dump_dot_packages filename pj =
  let graph = Ocamldot.create "Packages" [] in
  let nodes = ref StringMap.empty in
  Array.iter (fun pk ->
  let node = Ocamldot.node graph pk.package_name [] in
  nodes := StringMap.add pk.package_name node !nodes;
  List.iter (fun name ->
  nodes := StringMap.add pk.package_name node !nodes;
  )
  ( get_strings_with_default [pk.package_options] "provides"  [] )


  ) pj.project_sorted;
  Array.iter (fun pk0 ->
  let node0 = StringMap.find pk0.package_name !nodes in
  List.iter (fun dep ->
  let pk1 = dep.dep_project in
  let node1 = StringMap.find pk1.package_name !nodes in
  let (_ : Ocamldot.edge) = Ocamldot.edge node0 node1 [] in
  Printf.eprintf "%s -> %s\n%!" pk0.package_name pk1.package_name;
  ()
  ) pk0.package_requires
  ) pj.project_sorted;
  Ocamldot.save graph filename
*)

type package_comparison =
  PackageEquality
| PackageConflict
| PackageUpgrade of bool

let normalized_dir dir =
  File.to_string (File.of_string dir)

let is_enabled options =
  BuildValue.get_bool_with_default options "enabled" true

(*
module PackageLinkSorter = LinearToposort.Make(struct
  type t = final_package  package_dependency
  let node pd = pd.dep_project.pi.package_node
  let iter_edges f pd1 =
    IntMap.iter (fun _ pd2 ->
      if pd2.dep_link then f pd2) pd1.dep_project.pi.package_requires_map
  let name pd = pd.dep_project.package_name
  let debug = ref false
end)
*)

module PackageSorter = LinearToposort.Make(struct
  type t = pre_package
  let node pd = pd.package_node
  let iter_edges f pk =
    List.iter (fun pk2 -> f pk2) pk.package_requires_list
  let name pk = pk.package_name
  let debug = ref false
end)

  (*
let print_deps msg pk =
  Printf.eprintf "%s: Project %s depends on:\n%!" msg pk.package_name;
  List.iter (fun dep ->
    let pd = dep.dep_project in
    Printf.eprintf "\t%s %s%s%s%s\n%!"
      (string_of_package_type pd.package_type)
      pd.package_name
      (if dep.dep_link then "(link)" else "")
      (if dep.dep_syntax then "(syntax)" else "")
      (if dep.dep_optional then "(optional)" else "")
  ) pk.package_requires


let new_dep pk pk2 options =
  try
    IntMap.find pk2.package_id pk.pi.package_requires_map
  with Not_found ->
    let dep =
      {
        dep_project = pk2;
        dep_link = false;
        dep_syntax = false;
        dep_optional = false;
        dep_options = options;
      } in
    pk.pi.package_requires_map <-
      IntMap.add pk2.package_id dep pk.pi.package_requires_map;
    pk.pi.package_requires <- dep :: pk.pi.package_requires;
    (*    Printf.eprintf "New dep %s <- %s\n%!"
          pk2.package_name pk.package_name; *)
    dep
  *)

(*
  Do a closure of all dependencies for this project. Called only on
  validated_projects. The closure is useful in two cases:
  - for libraries, we need to include directories (-I) and link.
  - for syntaxes, we need also to include directories (-I) and link them,
  when calling the preprocessor.
  - we don't need more than that.
*)

  (*
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
    match dep.dep_project.package_type with
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
        dep.dep_project.package_name
        pj.package_name;
      exit 2;
  ) pj.pi.package_requires;

  (* add all link dependencies, transitively *)
  let rec add_link_deps pj1 =
    List.iter (fun dep ->
      let pj2 = dep.dep_project in
      let dep2 = new_dep pj pj2 dep.dep_options in
      if verbose 5 then
        Printf.eprintf "%S -> %S\n" pj.package_name pj2.package_name;
      if dep.dep_link &&
        (not dep2.dep_link || pj1 == pj) then begin
          dep2.dep_link <- true;
          if verbose 5 then
            Printf.eprintf "%S -> link %S\n" pj.package_name pj2.package_name;
          add_link_deps pj2
        end
    ) pj1.pi.package_requires
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
            Printf.eprintf "%S -> syntax %S\n" pj.package_name pj2.package_name;
          dep2.dep_syntax <- true;
          add_link_as_syntax_deps pj2
        end
    ) pj1.pi.package_requires
  in

  let add_syntax_deps pj1 =
    List.iter (fun dep ->
      if dep.dep_syntax then
        let pj2 = dep.dep_project in
        let dep2 = new_dep pj pj2 dep.dep_options in
        if not dep2.dep_syntax || pj1 == pj then begin
          dep2.dep_syntax <- true;
          if verbose 5 then
            Printf.eprintf "%S -> syntax %S\n" pj.package_name pj2.package_name;
          add_link_as_syntax_deps pj2;
        end
    ) pj1.pi.package_requires
  in
  add_syntax_deps pj;
  if !print_package_deps ||  verbose 5 then
    print_deps "AFTER update_deps SORT" pj;
  ()

  *)

let reset_package_ids _debug array =
  for i = 0 to Array.length array - 1 do
    (*    Printf.eprintf "reset_package_ids[%s] %s_%d -> %s_%d%!\n" debug
          array.(i).package_name array.(i).package_id array.(i).package_name i; *)
    array.(i).package_id <- i
  done

let requires_keep_order_option = BuildValue.new_bool_option "requires_keep_order" false

let plugin_verifiers = ref ([] : (BuildWarnings.set -> state -> unit) list)

let verify_packages w state =

  (*  Printf.eprintf "calling verifiers...%!"; *)
  List.iter (fun f ->
    f w state
  ) !plugin_verifiers;
  (* Printf.eprintf "done\n%!"; *)

  let _packages = final_state state in

  let sorted_packages = ref [] in
  let disabled_packages = ref [] in

  IntMap.iter (fun _ pk ->
    if pk.package_disabled then begin
      (* Printf.eprintf "BuildOCP: %S disabled\n%!" pk.package_name; *)
      disabled_packages := pk :: !disabled_packages
    end
    else begin
        (* Printf.eprintf "BuildOCP: %s_%d sorted \n%!"
           pk.package_name pk.package_id; *)
      sorted_packages := pk :: !sorted_packages
        end
  ) state.packages;

  (* Printf.eprintf "sorting...%!"; *)
  let (sorted_packages, cycle, non_sorted) =
    PackageSorter.sort !sorted_packages in
  (* Printf.eprintf "done\n%!"; *)

  let pj = {
    project_sorted = Array.of_list sorted_packages;
    project_disabled = Array.of_list !disabled_packages;
  } in

  (* Change the package IDs: the package_requires_map is not correct anymore ! *)
  reset_package_ids "project_sorted" pj.project_sorted;

  (*
  (* TODO: The impact of this is that all dependencies are sorted in
     the same order in all packages. This might, however, not be what
     someone wants, because you might want to have a different link
     order than the one globally inferred.  *)
  Array.iter (fun pk ->
    if requires_keep_order_option.get [pk.package_options]  then begin
      (* This option does not work.

      Printf.eprintf "pk=%s\n%!" pk.package_name;
      List.iter (fun dep ->
        Printf.eprintf "  pj=%s\n%!" dep.dep_project.package_name;
        IntMap.iter (fun _ dep ->
          Printf.eprintf "    pj=%s\n%!" dep.dep_project.package_name
        ) dep.dep_project.pi.package_requires_map;
        List.iter (fun name ->
          Printf.eprintf "     after: %s\n%!" name;
          try
            let pj2 = StringMap.find name pk.pi.package_deps_map in
            Printf.eprintf "pj2 = %s\n%!"
              pj2.dep_project.package_name;
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
      Printf.eprintf "pk=%s\n%!" pk.package_name;
      List.iter (fun dep ->
        Printf.eprintf "  pj=%s\n%!" dep.dep_project.package_name;
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
      Printf.eprintf "Package %S[%d]\n" pk.package_name pk.package_id;
      List.iter (fun dp ->
        Printf.eprintf "\t%S[%d]%s%s\n"
          dp.dep_project.package_name
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
  *)
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


type dot_ocpbuild = {
  mutable option_skip : bool;
  mutable option_maxversion : int;
}

let load_dot_ocpbuild filename c =
  FileString.iter_lines (fun line ->
      if line <> "" then
        if line.[0] <> '#' then
          let (key,value) = OcpString.cut_at line '=' in
          match key, value with
          | "skip", "true" -> c.option_skip <- true
          | "skip", "false" -> c.option_skip <- false
          | "maxversion", "1" -> c.option_maxversion <- 1;
          | "maxversion", "2" -> c.option_maxversion <- 2;
          | _ ->
            Printf.eprintf
              "Warning: in filename %s, unknown key/value pair:\n"
              filename;
            Printf.eprintf "     '%s=%s'\n%!" key value
    ) filename

let scan_root root_dir =
  let ocp_files = ref [] in

  (* We blacklist build.ocp if build.ocp2 exists. *)
  let blacklist = ref StringSet.empty in

  let queue = Stack.create () in

  let push_dir filename =
    let c = {
      option_skip = false;
      option_maxversion = 2;
    } in
    let ocp_file = Filename.concat filename ".ocp-build" in
    if Sys.file_exists ocp_file then load_dot_ocpbuild ocp_file c;
    if not c.option_skip then begin
      Stack.push filename queue;
      let ocp2_file = Filename.concat filename "build.ocp2" in
      if c.option_maxversion = 1 then begin
        blacklist := StringSet.add ocp2_file !blacklist
      end else begin
        if Sys.file_exists ocp2_file then
          blacklist := StringSet.add
              (Filename.concat filename "build.ocp") !blacklist
      end
    end
  in

  push_dir (File.to_string root_dir);
  while not (Stack.is_empty queue) do
    try
      let dirname = Stack.pop queue in
      let files = Sys.readdir dirname in
      Array.sort compare files;
      Array.iter (fun basename ->
          let filename = Filename.concat dirname basename in
          if (try Sys.is_directory filename with _ -> false) then begin

            let basename = Filename.basename filename in
            let initial = basename.[0] in
            if initial = '.' || initial = '_' ||
               Sys.file_exists (Filename.concat filename ".ocpstop") then
              () (* ignore directory *)
            else
              push_dir filename
          end else
            let basename = Filename.basename filename in
            match basename.[0] with
            | 'a'..'z' | 'A'..'Z' | '0'..'9' ->
              if not (StringSet.mem filename !blacklist) then begin
                if Filename.check_suffix filename ".ocp" then
                  let file = File.of_string filename in
                  ocp_files := file :: !ocp_files
                else
                if Filename.check_suffix filename ".ocp2" then
                  let file = File.of_string filename in
                  ocp_files := file :: !ocp_files
                else
                  ()
              end
            | _ -> ()
        ) files;
    with exn ->
      Printf.eprintf "Exception %s\n%!" (Printexc.to_string exn)
  done;
  List.rev !ocp_files
(* files are reverted, so that the first in breadth are used first
   (this is expected from [load_project] *)

let magic_head = "OCP-"
let magic_head_len = String.length magic_head
let magic_kind = "PROJ"
let magic_kind_len = String.length magic_kind
let magic_version = "20120928"
let magic_version_len = String.length magic_version
let magic = magic_head ^ magic_kind ^ magic_version
let magic_len = String.length magic

let save_project_state state filename =
  let oc = File.open_out_bin filename in
  output_string oc magic;
  output_value oc (state : project);
  close_out oc

let load_project_state filename =
  let ic = File.open_in_bin filename in
  let possible_magic =
    let magic = Bytes.create magic_len in
    try
            really_input ic magic 0 magic_len;
            Bytes.to_string magic
    with
    | _e ->
      close_in ic;
      failwith "load_project_state: truncated file"
  in
  if possible_magic <> magic then begin
    close_in ic;
    if String.sub possible_magic 0 magic_head_len <> magic_head then
      failwith "load_project_state: not an OCP file";
    if String.sub possible_magic magic_head_len magic_kind_len
      <> magic_kind then
      failwith "load_project_state: not an OCP PROJECT file";
    if String.sub possible_magic (magic_head_len + magic_kind_len)
      magic_version_len <> magic_version then
      failwith "load_project_state: bad OCP PROJECT version";
  end;
  try
    let v = (input_value ic : project) in
    close_in ic;
    v
  with e ->
    close_in ic;
    raise e

(*
  val scan_project : (project -> unit)
  let scan_project pj =
  let files = ref [] in
  BuildScanner.scan_directory_for_suffix
  (File.to_string pj.project_dir) ".ocp" (fun filename ->
  files := File.of_string filename :: !files);
  pj.project_files =:= List.rev !files;
  save_project pj.project_file pj;
  ()
*)

(*

  if !list_ocp_files || !verbosity_arg > 1 then begin
  Printf.eprintf "Found %d project files:\n%!" (List.length !files);
  List.iter (fun file ->
  Printf.eprintf "\t%s\n%!" file) !files;
  end;

*)

      (*
let find_package pj file =
  let list = ref [] in

  let st = File.stat file in
  (*
    let dir_t = pj.project_dir in
    let _dir = File.to_string dir_t in
  *)
  let check_file pk filename =
    let file = File.of_string (Filename.concat pk.package_dirname filename) in
    try
      let st2 = File.stat file in
      if
        st.MinUnix.st_ino = st2.MinUnix.st_ino &&
        st.MinUnix.st_dev = st2.MinUnix.st_dev then
        list := pk :: !list
    with _ -> ()
  in
  Array.iter (fun pk ->
    List.iter (fun (filename, _) ->
      check_file pk filename;
      let (_kernel, extension) = FileString.cut_at_last_extension filename in
      match extension with
      | "ml" -> check_file pk (filename ^ ".mli")
      | "mli" -> ()
      | "mll" -> check_file pk (filename ^ ".ml")
      | "mly" ->
        check_file pk (filename ^ ".ml");
        check_file pk (filename ^ ".mli")
      | _ -> ()
    ) (BuildValue.prop_list (BuildValue.get [pk.package_options] "files"))
  ) pj.project_sorted;

  !list
      *)

let rec find_obuild f dir =
  let possible_dir = Filename.concat dir "_obuild" in
  if Sys.file_exists possible_dir then
    f possible_dir
  else
    let new_dir = Filename.dirname dir in
    if dir <> new_dir then find_obuild f new_dir

(*
  val find_project : (File.t -> File.t)
*)
let find_root root_dir basenames =
  let rec find dirname (basenames : string list) =
    let file = File.add_basenames dirname basenames in
    if File.exists file then dirname else
      let new_dirname = File.dirname dirname in
      if new_dirname == dirname then raise Not_found;
      find new_dirname basenames
  in
  let root_dir = if File.is_absolute root_dir then root_dir else
      File.concat (File.getcwd ()) root_dir
  in
  find root_dir basenames

(*
  let rec eprint_project msg pj =
  Printf.eprintf "%s = {\n" msg;
  Printf.eprintf "  sorted = [\n";
  print_package_array "    " pj.project_sorted;
  Printf.eprintf "  ]\n";
  Printf.eprintf "}\n";

  and print_package_array indent array =
  let indent2 = indent ^ "  " in
  Array.iter (fun pk ->
  Printf.eprintf "%s{\n" indent;
  Printf.eprintf "%spackage_name = %S\n" indent2 pk.package_name;
  Printf.eprintf "%spackage_dirname = %S\n" indent2 pk.package_dirname;
  BuildOCPInterp.eprint_env indent2 pk.package_options;
  Printf.eprintf "%s}\n" indent;
  ) array
*)
