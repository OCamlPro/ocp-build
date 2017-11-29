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

(* Should be set to false in the future, when .ocp files become obsolete *)
let arg_load_ocp = ref true

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

let final_state state =
  if state.npackages = 0 then [||] else
    Array.init state.npackages (fun i ->
      { (IntMap.find i state.packages) with
        (*        pi = new_package_info (); *)
        package_requires_list = [];
      }
    )

let get_packages state = state.packages

let conf_disabled_packages = ref StringSet.empty
let conf_add_disabled_package dir_and_name =
  conf_disabled_packages := StringSet.add dir_and_name !conf_disabled_packages

let normalized_dir dir =
  FileGen.to_string (FileGen.of_string dir)

let new_package package_loc state name dirname
                filename filenames kind _options =
  let package_id = state.npackages in
    (* Printf.eprintf "new_package %s_%d\n%!" name package_id; *)
  state.npackages <- state.npackages + 1;
  let package_dirname = normalized_dir dirname in
  let dir_and_name = name ^ "@" ^ package_dirname in
  let pk = {
    package_source_kind = "ocp";
    package_id = package_id;
    package_loc;
    package_filename = filename;
    package_filenames = filenames;
    package_name = name;
    package_type = kind;
    package_dirname;
    package_plugin = Not_found;
    package_disabled =
      (if StringSet.mem dir_and_name !conf_disabled_packages ||
        StringSet.mem name !conf_disabled_packages then
        Some "Disabled by --disable"
      else None);
    package_requires_list = [];
    package_node = OcpToposort.new_node ();
  } in
  state.packages <- IntMap.add pk.package_id pk state.packages;
  pk

let empty_config () = BuildValue.empty_config ()
let generated_config () =
  let empty_config = empty_config () in
  {
    empty_config with
      config_env = BuildValue.set_bool empty_config.config_env "generated" true;
  }

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

let () =
  EvalOCP2.add_primitive "new_package" [
    "Create a new package: new_package(name, kind, ocaml)"
  ]
    (fun loc ctx config args ->
      match args with
      | [VString (name,_); VString (kind,_); VObject config_env] ->
        OCP_arg.define_package loc ctx { config  with config_env } ~name ~kind;
        VList []
      | _ ->
         BuildOCP2Prims.raise_bad_arity
           loc "new_package(string,string,object)" 3 args
    )

let add_primitive = EvalOCP2.add_primitive
let apply_fun = EvalOCP2.apply_fun
let primitives_help = EvalOCP1.primitives_help

let verbose = OcpDebug.verbose_function ["B";"BP"; "BuildOCP"]

let print_missing_deps = ref false

let init_packages () =
  let packages = initial_state () in
  packages

let print_loaded_ocp_files = ref false
let print_package_deps = ref false

(* Evaluate .ocp2 files: Module Files are evaluated first, then
       Description Files (build.ocp2 files). *)

let load_ocp_files config packages files =

  List.iter (fun file ->
    let file = FileGen.to_string file in
    let basename = Filename.basename file in
    if Filename.check_suffix file ".ocp2" &&
      String.lowercase basename <> "build.ocp2" then begin
        if verbose 5 || !print_loaded_ocp_files then
          Printf.eprintf "Reading module %s\n%!" file;
        let (_config : config) = EvalOCP2.read_ocamlconf file packages config in
        let modname = String.capitalize
          (Filename.chop_suffix basename ".ocp2") in
        if not (StringMap.mem modname config.config_state.cfs_modules)
          &&
            (let prefixed_modname =
               let dirname = Filename.basename (Filename.dirname file) in
               Printf.sprintf "%s:%s" dirname modname
             in
             not (StringMap.mem prefixed_modname
                    config.config_state.cfs_modules))
        then begin
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
        let file = FileGen.to_string file in
        if OcpString.starts_with file ~prefix:parent then
          let dirname = Filename.dirname file in
          if verbose 5 || !print_loaded_ocp_files then
            Printf.eprintf "Reading %s with context from %s\n%!" file filename;
          let config =
            try
              if Filename.check_suffix file ".ocp" && !arg_load_ocp then
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

module PackageSorter = OcpToposort.Make(struct
  type t = pre_package
  let node pd = pd.package_node
  let iter_edges f pk =
    List.iter (fun pk2 -> f pk2) pk.package_requires_list
  let name pk = pk.package_name
  let verbose = OcpDebug.verbose_function [ "BuildOCP.PackageSorter"]
end)

let reset_package_ids _debug array =
  for i = 0 to Array.length array - 1 do
    (*    Printf.eprintf "reset_package_ids[%s] %s_%d -> %s_%d%!\n" debug
          array.(i).package_name array.(i).package_id array.(i).package_name i; *)
    array.(i).package_id <- i
  done

    (*
let requires_keep_order_option =
  BuildValue.new_bool_option "requires_keep_order" false
     *)

let plugin_verifiers = ref ([] : (BuildWarnings.set -> state -> unit) list)

let html_header oc title =
  Printf.fprintf oc
"<!doctype html>\n\
<html xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\">\n\
<head>\n\
  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n\
  <link rel=\"stylesheet\" href=\"style.css\"/>\n\
  <title>%s</title></head><body>" title;
  Printf.fprintf oc
"<style type=\"text/css\" media=\"screen\">\
.enabled  { color: green; }\n\
.disabled  { color: red; }\n\
</style>"

type html_node = {
  dirs : html_node StringMap.t ref;
  pks: unit BuildOCPTypes.package list ref;
}

let html_report pj =
  BuildMisc.safe_mkdir "_obuild/_reports";
  let oc = open_out "_obuild/_reports/packages.html" in
  html_header oc "ocp-build: log of packages";
  Printf.fprintf oc "<h1>Project %s</h1>\n" (Sys.getcwd ());

  let html_report_package pk =
    Printf.fprintf oc "<a name='%s-%d'/>\n" pk.package_name pk.package_id;
    Printf.fprintf oc "<h3 class='%s'>Package %s</h3>\n"
      (if package_disabled pk then "disabled" else "enabled")
      pk.package_name;
    Printf.fprintf oc "(from <a href='%s'>%s</a>)\n"
      pk.package_filename pk.package_filename;
    match pk.package_disabled with
    | Some reason ->
      Printf.fprintf oc "<p>Disabled: %s</p>\n" reason
    | None ->
      Printf.fprintf oc "<p>Requires:\n";
      List.iter (fun pk ->
        Printf.fprintf oc "<a href='#%s-%d'>%s</a> "
          pk.package_name pk.package_id pk.package_name;
      ) pk.package_requires_list;
      Printf.fprintf oc "</p>\n";
  in

  let html_report_packages pks =
    let map = { dirs = ref StringMap.empty; pks = ref [] } in
    Array.iter (fun pk ->
      let dirname = pk.package_dirname in
      let path = OcpString.split dirname '/' in
      let rec iter path map =
        match path with
          [] -> map.pks := pk :: !(map.pks)
        | dir :: path ->
          let map =
            try
              StringMap.find dir !(map.dirs)
            with Not_found ->
              let map2 = { dirs = ref StringMap.empty; pks = ref [] } in
              map.dirs := StringMap.add dir map2 !(map.dirs);
              map2
          in
          iter path map
      in
      iter path map
    ) pks;

    let rec iter map =
      List.iter html_report_package !(map.pks);
      if not (StringMap.is_empty !(map.dirs)) then begin
        Printf.fprintf oc "<ul>\n";
        StringMap.iter (fun name map ->
          Printf.fprintf oc "<li>%s\n" name;
          iter map;
          Printf.fprintf oc "</li>\n";
        ) !(map.dirs);
        Printf.fprintf oc "</ul>\n";
      end
    in
    iter map;

  in

  Printf.fprintf oc "<h2>Enabled Packages</h2>\n";

  html_report_packages pj.project_sorted;

  Printf.fprintf oc "<h2>Disabled Packages</h2>\n";

  html_report_packages pj.project_disabled;

  close_out oc

let verify_packages w state =
  IntMap.iter (fun _ pk ->
    pk.package_requires_list <- [];
  ) state.packages;

(* Each plugin can verify and order its packages. For that, it can iter on
the state plugins `BuildOCP.get_packages state`, extract the specific
internal package description:
 `match pk.package_plugin with | OCamlPackage opk -> ...`
and it can modify:
* pk.package_disabled if the package should be disabled
* pk.package_requires_list to list the packages it depends on
  (for BuildOCP to do a topological sort)
*)
  List.iter (fun f ->
    f w state
  ) !plugin_verifiers;

  let _packages = final_state state in

  let sorted_packages = ref [] in
  let disabled_packages = ref [] in

  (*  Printf.eprintf "AFTER AFTER\n"; *)
  IntMap.iter (fun _ pk ->
(*    Printf.eprintf "  %b %s@%s\n" pk.package_disabled
      pk.package_name pk.package_dirname; *)
    if package_disabled pk then
      disabled_packages := pk :: !disabled_packages
    else
      sorted_packages := pk :: !sorted_packages
  ) state.packages;

  let (sorted_packages, _cycle, _non_sorted) =
    PackageSorter.sort !sorted_packages in

  let pj = {
    project_sorted = Array.of_list sorted_packages;
    project_disabled = Array.of_list !disabled_packages;
  } in

  (* Change the package IDs: the package_requires_map is not correct anymore ! *)
  reset_package_ids "project_sorted" pj.project_sorted;
  reset_package_ids "project_disabled" pj.project_disabled;

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

  push_dir (FileGen.to_string root_dir);
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
                  if Filename.check_suffix filename ".ocp" && !arg_load_ocp then
                  let file = FileGen.of_string filename in
                  ocp_files := file :: !ocp_files
                else
                if Filename.check_suffix filename ".ocp2" then
                  let file = FileGen.of_string filename in
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
  let oc = FileGen.open_out_bin filename in
  output_string oc magic;
  output_value oc (state : project);
  close_out oc

let load_project_state filename =
  let ic = FileGen.open_in_bin filename in
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

let rec find_obuild f dir =
  let possible_dir = Filename.concat dir "_obuild" in
  if Sys.file_exists possible_dir then
    f possible_dir
  else
    let new_dir = Filename.dirname dir in
    if dir <> new_dir then find_obuild f new_dir

let find_root root_dir basenames =
  let rec find dirname (basenames : string list) =
    let file = FileGen.add_basenames dirname basenames in
    if FileGen.exists file then dirname else
      let new_dirname = FileGen.dirname dirname in
      if new_dirname == dirname then raise Not_found;
      find new_dirname basenames
  in
  let root_dir = if FileGen.is_absolute root_dir then root_dir else
      FileGen.concat (FileGen.getcwd ()) root_dir
  in
  find root_dir basenames
