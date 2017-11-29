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

(* TODO:
  - Support for shared libraries. Currently, ocp-build ONLY supports building
   in custom mode. Shared mode is a bit more complex, as linking with a library
   would depend on different files, if a shared library or a static library is
   built.
*)

(*
ocp-imports should be able to print this !

Warning 40: package_options was selected from type BuildOCPTypes.package.
Warning 40: package_dirname was selected from type BuildOCPTypes.package.
Warning 40: package_options was selected from type BuildOCPTypes.package.
Warning 40: package_name was selected from type BuildOCPTypes.package.

*)



(* special attributes *)
let mli_file_attr = "mli_file"
let file2string_attr = "file2string"
let binannot_attr = "binannot"


open OcpCompat

open BuildMisc

open BuildEngineTypes
open BuildEngineGlobals
open BuildEngineContext
open BuildEngineRules

open BuildValue.TYPES

open BuildTypes
open BuildGlobals
open BuildOCamlConfig

open BuildOCamlTypes
open BuildOCamlVariables
open BuildOCamlMisc
open BuildOCamlInstall.TYPES

(* renamed to record in replay log *)
let safe_mkdir (dir : string) =
  BuildEngineReport.cmd_mkdir dir;
  BuildMisc.safe_mkdir dir

let add_file lib dir name =
  BuildEngineContext.add_file lib.lib.lib_package dir name

let add_temp_file lib dir name =
  BuildEngineContext.add_temp_file lib.lib.lib_package dir name

let add_virtual_file lib dir name =
  BuildEngineContext.add_virtual_file lib.lib.lib_package dir name

let add_dst_file lib dir name =
  BuildOCamlMisc.add_dst_file lib.lib.lib_package dir name


let comp_deps w lib options =
  let options = options :: lib.lib_opk.opk_options in
  let comp_requires =  comp_requires_option.get options in
  BuildOCamlSyntaxes.get_tool_requires w "comp" lib comp_requires

let string_of_libloc lib =
  Printf.sprintf "File %S, line 0, characters 0-1:\nPackage %S:"
    lib.lib.lib_filename lib.lib.lib_name

(* TODO: [mut_dir] does not work for source files beginning with ".."
   and for source files in other packages (package = "toto")
*)


let ocamlc_command options ocamlc_specific ocamlc_generic =
  let ocamlc_command = ocamlc_specific.get options in
  if ocamlc_command = [] then
    ocamlc_generic.get options
  else ocamlc_command

let copy_dir lib src_file =
  let b = lib.lib.lib_context in
  let mut_dirname =
    Filename.concat b.build_dir_filename "_mutable_tree" in
  safe_mkdir mut_dirname;
  let mut_dir = BuildEngineContext.add_directory b mut_dirname in
(*
  let rec iter mut_dir file_dir =
    (*
    Printf.eprintf "src_dir = %S\n%!" lib.lib.lib_src_dir.dir_fullname;
    Printf.eprintf "fil_dir = %S\n%!" file_dir.dir_fullname;
    Printf.eprintf "mut_dir = %S\n%!" lib.lib_mut_dir.dir_fullname;
*)
    if file_dir.dir_parent == file_dir
    then mut_dir else
      let parent_dir = file_dir.dir_parent in
(*      Printf.eprintf "check parent\n"; *)
(*      assert (lib.lib_mut_dir.dir_fullname <> file_dir.dir_fullname); *)
      let mut_dir = iter mut_dir parent_dir in
      let subdir = Filename.concat mut_dir.dir_fullname file_dir.dir_basename
      in
      safe_mkdir subdir;
      add_directory lib.lib.lib_context subdir
  in
*)
  try
    let subdir = Filename.concat mut_dir.dir_fullname
      src_file.file_dir.dir_basename in
      safe_mkdir subdir;
    let copy_dir = BuildEngineContext.add_directory
      lib.lib.lib_context subdir in

    (*    let copy_dir = iter mut_dir  src_file.file_dir in *)
(*    Printf.eprintf "COPY DIR of %S is %S\n%!"
      (FileGen.to_string src_file.file_file) copy_dir.dir_fullname; *)
    copy_dir
  with Stack_overflow ->
    Printf.eprintf "Error: Stack_overflow while computing mut_dir\n";
    Printf.eprintf "  of source file %S of package %S \n%!"
      (FileGen.to_string src_file.file_file)
      lib.lib.lib_name;
    clean_exit 2

let verbose = OcpDebug.verbose_function ["B"; "BuildOCamlRules"]

let chop_prefix s prefix =
  let prefix_len = String.length prefix in
  String.sub s prefix_len (String.length s - prefix_len)

type package_temp_variables = {
  mutable src_files : build_file IntMap.t;
  mutable dep_files : build_file IntMap.t;
  cmi_files : build_file list ref;
  cmo_files : build_file list ref;
  odoc_files : build_file list ref;
  cmx_files : build_file list ref;
  cmx_o_files : build_file list ref;
  o_files : build_file list ref;
}

let new_package_temp_variables () = {
  src_files =  IntMap.empty;
  dep_files =  IntMap.empty;
  cmi_files =  ref [];
  cmo_files =  ref [];
  odoc_files =  ref [];
  cmx_files =  ref [];
  cmx_o_files = ref []; (* .o files generated with .cmx files *)
  o_files = ref [];
}

(* TODO: must do something more correct !! *)
let ocaml_version_greater_than version options =
  let ocaml_version = ocaml_config_version.get options in
  ocaml_version >= version

let add_bin_annot_argument cmd options =
  if ocaml_version_greater_than "4" options &&
     BuildValue.get_bool_with_default options binannot_attr true
  then
    add_command_args cmd [S "-bin-annot" ]

let c_includes lib =
  let added_dirs = ref IntMap.empty in
  let includes = ref [] in
  let add_include_dir dir =
    if not (IntMap.mem dir.dir_id !added_dirs) then begin
      added_dirs := IntMap.add dir.dir_id dir !added_dirs;
      includes := !includes @ [S "-I"; S  dir.dir_fullname];
    end
  in

  add_include_dir lib.lib.lib_src_dir;

  (* TODO: Fabrice: they should be reversed, no ?
     We should search directories in the
     reverse order of the topological order. *)
  List.iter (fun dep ->
    let lib = dep.dep_project in
    match lib.lib.lib_type with
    | ProgramPackage (* | ProjectToplevel *) -> ()
    | TestPackage -> assert false
    | LibraryPackage
    | ObjectsPackage
    | RulesPackage
      ->
      if dep.dep_link || externals_only.get [dep.dep_options]
      then begin
        add_include_dir lib.lib.lib_src_dir;
      end
    | SyntaxPackage -> ()
  ) (List.rev lib.lib_requires);
  !includes

let command_includes lib pack_for =
  let includes =
    match lib.lib_includes with
    | Some includes -> includes
    | None ->

      let added_dirs = ref IntMap.empty in
      let includes = ref [] in
      let add_include_dir dir =
        if not (IntMap.mem dir.dir_id !added_dirs) then begin
          added_dirs := IntMap.add dir.dir_id dir !added_dirs;
          includes := !includes @ ["-I"; dir.dir_fullname];
        end
      in

      add_include_dir lib.lib.lib_dst_dir;
      add_include_dir lib.lib.lib_src_dir;

   (* TODO: Fabrice: they should be reversed, no ?
      We should search directories in the
      reverse order of the topological order. *)
      List.iter (fun dep ->
        let lib = dep.dep_project in
        match lib.lib.lib_type with
        | ProgramPackage (* | ProjectToplevel *) -> ()
        | TestPackage -> assert false
        | LibraryPackage
        | ObjectsPackage
          ->
          if dep.dep_link || externals_only.get [dep.dep_options] then begin
            add_include_dir lib.lib.lib_dst_dir;
            add_include_dir lib.lib.lib_src_dir;
          end
        | SyntaxPackage -> ()
        | RulesPackage ->
          add_include_dir lib.lib.lib_src_dir;
          add_include_dir lib.lib.lib_dst_dir;
      ) (List.rev lib.lib_requires);

   (* we put the source dir last in case there are some remaining objects files there, since
      we don't do any hygienic cleaning before. We don't do it because we want to be able to
      support object files that are built by other means. *)

      let includes = !includes in
      lib.lib_includes <- Some includes;
      includes
  in
  let rec add_internal_includes pack_for includes =
    match pack_for with
      [] -> includes
    | _ :: tail ->
      let includes = add_internal_includes tail includes in
      "-I" :: (Filename.concat lib.lib.lib_dst_dir.dir_fullname
                 (String.concat "/" (List.rev pack_for))) ::
        includes
  in
  add_internal_includes (List.rev pack_for) includes

(*
let command_pp ptmp options =
  match string_option options pp_option with
    | "" -> []
    | pp -> ["-pp"; pp]
*)

let add_package_file lib filename =
  let b = lib.lib.lib_context in
  if Filename.is_relative filename then
    add_file lib lib.lib.lib_src_dir filename
  else
    let dir =
      let dirname = Filename.dirname filename in
      try
        find_directory b dirname
      with Not_found ->
        Printf.eprintf "Error: directory %S of %S is not a package directory\n%!" dirname lib.lib.lib_name;
        exit 2
    in
    add_file lib dir (Filename.basename filename)

let add_more_rule_sources lib r deps options =
  let more_rule_sources =  rule_sources_option.get options
    @  more_deps_option.get options  in
  List.iter (fun s ->
    let s = BuildSubst.subst_global s in
    let s = add_package_file lib s in
    add_rule_source r s
  ) more_rule_sources;
  List.iter (fun option ->
    List.iter (fun s ->
      let s = BuildSubst.subst_global s in
      let s = add_package_file lib s in
      add_rule_source r s
    ) (option.get options)
  ) deps

    (*
let add_objects lib name_objs options =
  List.map (fun s ->
    let s = BuildSubst.subst_global s in
    add_package_file lib s)
    (BuildValue.get_strings_with_default options name_objs [])
    *)

(* override [new_rule] to add [lib_ready] *)
let new_rule lib file cmds =
  let r = new_rule lib.lib.lib_context lib.lib.lib_loc file cmds in
  add_rule_sources r lib.lib_ready;
  r

let add_c2o_rule b lib seq src_file target_file options =
  let build_dir = BuildEngineContext.add_directory b (MinUnix.getcwd ()) in
  let temp_file = add_temp_file lib build_dir target_file.file_basename in
  let r = new_rule lib target_file
    [Execute (new_command

      ( ocamlcc_cmd.get options
(*
             (if bool_option_true lib.lib.lib_opk.opk_options byte_option then ocamlcc_cmd
             else ocamlopt_cmd) *)
                )
      (c_includes lib @[
       S "-ccopt"; S
                   (String.concat " " (cflags_option.get  options));
       S "-ccopt"; S (String.concat " " ( ccopt_option.get options));
       S "-c"; S (file_filename src_file);
      ])
);
     Move (false, F temp_file.file_file, F target_file.file_file)
    ]
  in
  add_more_rule_sources lib r [] options;
  add_rule_source r src_file;
  add_rule_sources r seq;
  add_rule_temporary r temp_file

let add_mll2ml_rule lib src_file target_file options =

  let envs = options :: lib.lib_opk.opk_options in

  let r = new_rule lib target_file
    [Execute (new_command (ocamllex_cmd.get envs )
                [ S "-o"; BF target_file; BF src_file])
    ]
  in
  add_more_rule_sources lib r [ ocamllex_deps ] envs;
  add_rule_source r src_file

let add_mly2ml_rule lib src_file ml_target_file mli_target_file options =

  let envs = options :: lib.lib_opk.opk_options in
  let src_dir = src_file.file_dir in
  let temp_ml = add_temp_file lib src_dir ml_target_file.file_basename in
  let temp_mli = add_temp_file lib src_dir mli_target_file.file_basename in
  let r = new_rule lib ml_target_file
    [Execute (new_command ( ocamlyacc_cmd.get envs) [BF src_file]);
     Move (false, BF temp_ml, BF ml_target_file);
     Move (false, BF temp_mli, BF mli_target_file);
    ]
  in
  add_more_rule_sources lib r [ ocamlyacc_deps ] envs;
  add_rule_source r src_file;
  add_rule_target r mli_target_file

(* reading dependencies is a bit complicated, as the format of make
   dependencies is not OK on Windows (because : is used in
   filenames). We should fix filenames in those cases.
  Note that ocamldep will escape any space in a filename, so that
  spaces are the only significant characters.
  Read the full file. Convert \\\n sequences into spaces.


   Instead, we should have a specail format, such as:

   CMO filename
   DEP dependency
   DEP dependency

  TODO: add a String.unescaped, the inverse of String.escaped.
*)


let add_flag option flag options flags =
  if option.get options && not (List.mem (S flag) flags) then
    (S flag) :: flags else flags

let add_nopervasives_flag = add_flag nopervasives "-nopervasives"
let add_asmdebug_flag = add_flag asmdebug_option "-g"
let add_bytedebug_flag = add_flag bytedebug_option "-g"
let add_debug_flag = add_flag debug_option "-g"

let bytelinkflags lib =
  let options = lib.lib_opk.opk_options in
  add_debug_flag options (
    add_bytedebug_flag options (
      add_nopervasives_flag options (
        List.map argument_of_string  (bytelink_option.get options)
      )
    )
  )

let asmlinkflags lib =
  let options = lib.lib_opk.opk_options in
  add_debug_flag options (
    add_asmdebug_flag options (
      add_nopervasives_flag options (
        List.map argument_of_string (asmlink_option.get options )
      )
    )
  )

let depflags options =
    List.map argument_of_string ( dep_option.get options)
let bytecompflags options =
  add_debug_flag options (
  add_bytedebug_flag options (
    add_nopervasives_flag options (
    List.map argument_of_string ( bytecomp_option.get options)
    )))
let docflags options =
  add_nopervasives_flag options (
    List.map argument_of_string ( doc_option.get options)
  )
let asmcompflags options =
  add_debug_flag options (
  add_asmdebug_flag options (
    add_nopervasives_flag options (
    List.map argument_of_string (asmcomp_option.get options )
    )))

let needs_odoc lib =
  match lib.lib.lib_type with
  LibraryPackage | ObjectsPackage -> true
  | ProgramPackage | SyntaxPackage
  | TestPackage | RulesPackage -> false

let add_ml2mldep_rule lib dst_dir pack_for force src_file target_file needs_odoc options =
  let envs = options :: lib.lib_opk.opk_options in
  let cmd = new_command (ocamldep_cmd.get envs)
    (depflags envs) in
  add_command_string cmd "-modules";
  add_command_strings cmd (command_includes lib pack_for);
(*  add_command_strings cmd (command_pp lib options); *)
  if force = Force_IMPL || ml_file_option.get envs  then
    add_command_strings cmd [ "-impl" ]
  else
  if force = Force_INTF || mli_file_option.get envs  then
    add_command_strings cmd [ "-intf" ]
  ;
  add_command_strings cmd [file_filename src_file];
  add_command_pipe cmd (file_filename target_file);

  let r = new_rule lib target_file [Execute cmd] in


  add_more_rule_sources lib r [ ocamldep_deps ] envs;
  add_rule_source r src_file;
  (* We don't need to have all the sources available ! Actually, the
     computation of dependencies is not done on the file-system, but on
     the virtual image of the file system, so files don't need to be
     present, they just need to be known to exist...

     List.iter (fun pd ->
      let lib = pd.dep_project in
      IntMap.iter (fun _ file -> add_rule_source r file) lib.lib_dep_deps
     ) lib.lib.lib_requires;
  *)

  let mldep_file_loaded = add_virtual_file lib dst_dir
      (target_file.file_basename ^ " loaded") in

(*
  let mldep_file_ok = add_virtual_file b dst_dir
      (target_file.file_basename ^ " ok") in

  let r_ok = new_rule b lib.lib_loc mldep_file_ok [] in
  r_ok.rule_forced <- true;  (* must be executed, even when no changes *)
  add_rule_source r_ok mldep_file_loaded;
*)

  let loader =
    BuildOCamldep.load_modules_dependencies
      lib options force dst_dir pack_for needs_odoc
  in
  let r_loaded = new_rule lib mldep_file_loaded [] in
  add_rule_command r_loaded (LoadDeps (loader, target_file, r_loaded));
  r_loaded.rule_forced <- true; (* must be executed, even when no changes *)
  add_rule_source r_loaded target_file;

  mldep_file_loaded






type 'a to_sort =
    {
      to_sort_value : 'a;
      to_sort_node : OcpToposort.node;
      mutable to_sort_deps : 'a to_sort list;
    }

module FileSorter = OcpToposort.Make(struct
  type t = build_file to_sort
  let node to_sort = to_sort.to_sort_node
  let iter_edges f to_sort = List.iter f to_sort.to_sort_deps
  let name to_sort = file_filename to_sort.to_sort_value
  let verbose = OcpDebug.verbose_function [ "BuildOCamlRules.FileSorter" ]
end)

(* We use the graph of build rules to sort topologically the object files *)
let sort_ocaml_files lib cmo_files =

  if verbose 3 then begin
    Printf.eprintf "Sorting:\n";
    List.iter (fun file ->
      Printf.eprintf "%s " file.file_basename;
    ) cmo_files;
  end;

  let map = ref StringMap.empty in
  let list = ref [] in
  let cmo_files = List.map (fun file ->
    let modname = Filename.chop_extension file.file_basename in
    let modname = String.capitalize modname in
    let to_sort =  {
      to_sort_value = file;
      to_sort_node = OcpToposort.new_node();
      to_sort_deps = [];
    } in
    map := StringMap.add modname to_sort !map;
    list := to_sort :: !list;
    (file, to_sort)
  ) cmo_files in

  (* reverse to keep original order *)
  let list = List.rev !list in
  List.iter (fun (file, to_sort) ->
    List.iter (fun r ->
      if r.rule_state <> RULE_INACTIVE then
        IntMap.iter (fun _ file2 ->
          try
            let modname =
              try Filename.chop_extension file2.file_basename
              with _ -> raise Not_found in
            let modname = String.capitalize modname in
            let to_sort2 = StringMap.find modname !map in
            if to_sort2 != to_sort then
              to_sort.to_sort_deps <- to_sort2 :: to_sort.to_sort_deps
          with Not_found -> ()
        ) r.rule_sources
    ) file.file_target_of
  ) cmo_files;

  let (sorted, cycle, _others) = FileSorter.sort list in
  if cycle <> [] then begin
    Printf.eprintf
      "Error: There is a cycle in the inter-dependencies inside package %S.\n"
    lib.lib.lib_name;
    Printf.eprintf
      " You MUST specify the link order of modules by ordering 'files'\n";
    Printf.eprintf
      " and using 'sort=false' in the package description.\n%!";
    exit 2
  end;
  let cmo_files =
    List.map (fun to_sort -> to_sort.to_sort_value) sorted in

  if verbose 3 then begin
    Printf.eprintf "\n";
    Printf.eprintf "Sorted:\n";
    List.iter (fun file ->
      Printf.eprintf "%s " file.file_basename;
    ) cmo_files;
    Printf.eprintf "\n";
  end;
  cmo_files




let add_files_to_link_to_command lib case cmd options cmx_files =
  if sort_files_option.get options  then begin
    DynamicAction (
      (Printf.sprintf "sort for %s" case),
      lazy (
        let cmx_files = sort_ocaml_files lib cmx_files in
        List.iter (fun cmx_file ->
          add_command_args cmd [BF cmx_file]) cmx_files;
        [Execute cmd]
      )
    )
  end else begin
    List.iter (fun cmx_file ->
      add_command_args cmd [BF cmx_file]) cmx_files;
    Execute cmd
  end




let add_cmo2cma_rule lib ptmp cclib cmo_files cma_file =
  if not lib.lib_opk.opk_installed then
    let options = lib.lib_opk.opk_options in
    let cmd = new_command
      (ocamlc_command options ocamlc2cma_cmd ocamlc_cmd
      ) (bytelinkflags lib) in
    add_command_args cmd  [S "-a"; S "-o"; BF cma_file];
    if cclib <> "" then
      add_command_strings cmd [ "-custom" ; "-cclib"; cclib ];
    if force_link_option.get options then
      add_command_strings cmd [ "-linkall" ];

    let cmd = add_files_to_link_to_command lib "byte lib" cmd options cmo_files in
    let r = new_rule lib cma_file [cmd] in
    add_more_rule_sources lib r [ ocamlc_deps; bytelink_deps; link_deps ] options;
    add_rule_sources r cmo_files;
    add_rule_sources r !(ptmp.cmi_files)

let cross_move r list =
  r.rule_commands <- r.rule_commands @
    (List.map (fun (f1, f2) ->
      Move (false, f1, f2)
     ) list)

let cross_update r list =
  r.rule_commands <- r.rule_commands @
    (List.map (fun (f1, f2) ->
      Move (true, f1, f2)
     ) list)

let add_cmx2cmxa_rule lib cclib cmi_files cmx_files cmx_o_files stubs_files =
  let options = lib.lib_opk.opk_options in
  let src_dir = lib.lib.lib_src_dir in
  let dst_dir = lib.lib.lib_dst_dir in

  let basename_cmxa = lib.lib_archive ^ ".cmxa" in
  let basename_cmxs = lib.lib_archive ^ ".cmxs" in
  let ext_lib = BuildOCamlConfig.ocaml_config_ext_lib.get options  in
  let basename_a = lib.lib_archive ^ ext_lib in

  let cmxa_file = add_dst_file lib dst_dir basename_cmxa in
  let a_file = add_dst_file lib dst_dir basename_a in
  let cmxs_file = add_dst_file lib dst_dir basename_cmxs in

  let has_cmxs = cmxs_plugin.get options in
  let cmxs_files = if has_cmxs then [cmxs_file, CMXS] else [] in

  if not lib.lib_opk.opk_installed then begin

    (* Build the cmxa *)
    let temp_cmxa = add_temp_file lib src_dir basename_cmxa in

    let r = new_rule lib cmxa_file [] in

    let temp_a = add_temp_file lib src_dir basename_a in

    begin
      let cmd = new_command (ocamlopt_cmd.get options ) (asmlinkflags lib) in
      add_command_args cmd [S "-a"; S "-o"; BF temp_cmxa ];
      if cclib <> "" then
        add_command_strings cmd ["-cclib"; cclib];
      if  force_link_option.get options then
        add_command_strings cmd [ "-linkall" ];

      let cmd = add_files_to_link_to_command lib "asm lib" cmd options cmx_files in
      add_rule_command r cmd;
      add_rule_target r a_file;
      add_rule_temporaries r [ temp_cmxa; temp_a ];

    end;

    add_more_rule_sources lib r
      [ ocamlopt_deps; asmlink_deps; link_deps ] options;
    add_rule_sources r cmx_files;
    add_rule_sources r cmx_o_files;
    add_rule_sources r cmi_files;
    cross_move r [ F temp_cmxa.file_file, F cmxa_file.file_file;
                   F temp_a.file_file, F a_file.file_file;
                 ];

    if has_cmxs then begin

        let temp_cmxs = add_temp_file lib src_dir basename_cmxs in

        let asmlink_libs =
          List.map (fun s ->
            let s = BuildSubst.subst_global s in
            add_package_file lib s
          ) (asmlink_libs.get options) in

        let cmd = new_command
          (ocamlopt_cmd.get options )
          (
            (asmlinkflags lib) @
              [S "-shared"; S "-I";
                              S lib.lib.lib_dst_dir.dir_fullname;
                              S "-o"; BF temp_cmxs ] @
              (if cclib = "" then [] else [S "-cclib"; S cclib]) @
              (if force_link_option.get options then
                  [S "-linkall" ] else [] ) @
              (List.map (fun f -> BF f) asmlink_libs)
          )
        in

        let cmd = add_files_to_link_to_command lib "cmxs lib"
          cmd options cmx_files in

        (* We can probably not build the .cmxs in parallel with the .cmxa.
           So, we just do then consecutively in the same rule. *)
        add_rule_command r cmd;
        add_rule_sources r asmlink_libs;
        add_rule_target r cmxs_file;
        add_rule_temporaries r [ temp_cmxs ];
        add_more_rule_sources lib r
          [ ocamlopt_deps; asmlink_deps; link_deps ] options;
        add_rule_sources r stubs_files;
      (* TODO: as we introduce this new dependency, we might want to
         split generation of .cmxa from .cmxs to be able to do them in
         parallel *)
        cross_move r [
          F temp_cmxs.file_file, F cmxs_file.file_file;
        ];
    end;
  end;
  (cmxa_file, a_file, cmxs_files)



let add_odocs2html_rule lib odoc_files docdir html_file =
  if not lib.lib_opk.opk_installed then
    let options = lib.lib_opk.opk_options in
    let cmd = new_command (ocamldoc_cmd.get options ) [] in
    List.iter (fun odoc_file ->
      add_command_args cmd [S "-load"; BF odoc_file]
    ) odoc_files;
    add_command_args cmd [S "-html";S "-d"; BD docdir];

    let r = new_rule lib html_file [Execute cmd] in
    add_more_rule_sources lib r [ ocamldoc_deps ] options;
    add_rule_sources r odoc_files

let get_link_order lib =
  let tolink =
    List.fold_right (fun pd links ->
      if pd.dep_link then
        let lib2 = pd.dep_project in
        lib2 :: links
      else links)
      lib.lib_requires []
  in

  let link_order = link_order.get lib.lib_opk.opk_options in
  if link_order = [] then tolink else
    let map = List.fold_left (fun map lib ->
      StringMap.add lib.lib.lib_name (lib, ref false) map
    )  StringMap.empty tolink
    in
    let tolink =
      List.map (fun name -> try
                              let (lib, used) = StringMap.find name map in
                              used := true;
                              lib
        with Not_found ->
          Printf.eprintf "Error with package %S: %S in 'link_order' is not specified in 'requires'\n%!" lib.lib.lib_name name;
          exit 2
      ) link_order
    in
    StringMap.iter (fun name (_, used) ->
      if not !used then
        Printf.eprintf "Warning with package %S: required %S not specified in `link_order'\n%!" lib.lib.lib_name name
    ) map;
    tolink

let add_cmo2byte_rule lib ptmp linkflags cclib cmo_files o_files byte_file =
  if not lib.lib_opk.opk_installed then
    let options = lib.lib_opk.opk_options in
    let ocamlc_command =
      if is_toplevel.get options then
        ocamlmktop_cmd.get options
      else
        ocamlc_command options ocamlc2byte_cmd ocamlc_cmd
    in
    let cmd = new_command ocamlc_command linkflags in
    add_command_args cmd [S "-o"; BF byte_file];
    let custom = ref false in
    List.iter (fun o_file ->
      custom := true;
      add_command_args cmd [BF o_file]) o_files;
    if cclib <> "" then
      add_command_args cmd [S "-cclib"; S cclib ];
    add_command_strings cmd (command_includes lib []);

    (*    Printf.eprintf "to_link for %S\n%!"  lib.lib_name; *)
    List.iter (fun lib2 ->
      match lib2.lib.lib_type with
      | LibraryPackage
      | RulesPackage
      | ObjectsPackage
      | ProgramPackage ->
        add_command_args cmd (bytelinkflags lib2);
        if not lib2.lib_meta then begin
          let has_ocaml_modules = ref false in
          List.iter (fun (obj, kind) ->
            match kind with
            | CMA
            | CMO ->
               has_ocaml_modules := true;
               add_command_arg cmd (BF obj)
            | _ -> ()
          ) lib2.lib_byte_targets;
          if not (lib2.lib_autolink && !has_ocaml_modules) then
            List.iter (fun (obj, kind) ->
              match kind with
              | STUB_A -> add_command_arg cmd (BF obj)
              (*     [S "-cclib"; S ("-l" ^ lib2.lib_stubarchive)] *)
              | _ -> ()
            ) lib2.lib_stub_targets;
        end;
      | SyntaxPackage -> ()
      | TestPackage -> ()
    ) lib.lib_linkdeps;
    if !custom then add_command_string cmd "-custom";

    let bytelink_libs =
      List.map (fun s ->
        let s = BuildSubst.subst_global s in
        add_package_file lib s
      ) (bytelink_libs.get options) in

    List.iter (fun s -> add_command_arg cmd (BF s)) bytelink_libs;

    let cmd = add_files_to_link_to_command lib "byte prog" cmd options cmo_files in
    let r = new_rule lib byte_file [cmd] in
    add_more_rule_sources lib r [ ocamlc_deps; bytelink_deps; link_deps ] options;

    add_rule_sources r cmo_files;
    add_rule_sources r !(ptmp.cmi_files);
    add_rule_sources r o_files;
    List.iter (fun lib2 ->
      List.iter (fun (obj, kind) ->
        match kind with
        | CMA | CMO -> add_rule_source r obj
        | _ -> ()
      ) lib2.lib_byte_targets;
      List.iter (fun (obj, kind) ->
        match kind with
        | STUB_A -> add_rule_source r obj
        | _ -> ()
      ) lib2.lib_stub_targets;
    ) lib.lib_linkdeps;
    add_rule_sources r bytelink_libs


let add_cmx2asm_rule lib ptmp linkflags cclib cmx_files cmx_o_files o_files opt_file =
  if  not lib.lib_opk.opk_installed then
    let options = lib.lib_opk.opk_options in
    let cmd = new_command
      (ocamlc_command options ocamlopt2asm_cmd ocamlopt_cmd)
      linkflags in
    add_command_args cmd [S "-o"; BF opt_file];
    if cclib <> "" then
      add_command_args cmd [S "-cclib"; S cclib];
    List.iter (fun o_file ->
      add_command_arg cmd (BF o_file)) o_files;
    add_command_strings cmd (command_includes lib []);

    (*    Printf.eprintf "To link %S:\n%!" lib.lib_name; *)
    List.iter (fun lib2 ->
      (*      Printf.eprintf "  Lib %S\n%!" lib2.lib_name; *)
      match lib2.lib.lib_type with
      | LibraryPackage
      | RulesPackage
      | ObjectsPackage
      | ProgramPackage ->
         add_command_args cmd (asmlinkflags lib2);
         let has_ocaml_modules = ref false in
         List.iter (fun (obj, kind) ->
             match kind with
             | CMXA | CMX ->
                has_ocaml_modules := true;
                add_command_arg cmd (BF obj)
            | _ -> ()
          ) lib2.lib_asm_targets;
          if not (!has_ocaml_modules && lib2.lib_autolink) then
            List.iter (fun (obj, kind) ->
              match kind with
              | STUB_A -> add_command_arg cmd (BF obj)
              (*     [S "-cclib"; S ("-l" ^ lib2.lib_stubarchive)] *)
              | _ -> ()
            ) lib2.lib_stub_targets;
      | SyntaxPackage -> ()
      | TestPackage -> ()
    ) lib.lib_linkdeps;

    let asmlink_libs =
      List.map (fun s ->
        let s = BuildSubst.subst_global s in
        add_package_file lib s
    ) (asmlink_libs.get options) in

    List.iter (fun s -> add_command_arg cmd (BF s)) asmlink_libs;

    let cmd = add_files_to_link_to_command lib "asm prog" cmd options cmx_files in
    let r = new_rule lib opt_file [cmd] in
    add_more_rule_sources lib r [ ocamlopt_deps; asmlink_deps; link_deps ] options;

    add_rule_sources r cmx_files;
    add_rule_sources r cmx_o_files;
    add_rule_sources r !(ptmp.cmi_files);
    add_rule_sources r o_files;
    List.iter (fun lib2 ->
      List.iter (fun (obj, kind) ->
        match kind with
        | CMXA | CMXA_A
        | CMX | CMX_O -> add_rule_source r obj
        | _ -> ()
      ) lib2.lib_asm_targets;
      List.iter (fun (obj, kind) ->
        match kind with
        | STUB_A -> add_rule_source r obj
        | _ -> ()
      ) lib2.lib_stub_targets;
    ) lib.lib_linkdeps;
    add_rule_sources r asmlink_libs;
    ()

let add_os2a_rule lib o_files a_file =
  let envs = lib.lib_opk.opk_options in

  if not lib.lib_opk.opk_installed then
    let target = a_file.file_basename in
    let ext_lib = BuildOCamlConfig.ocaml_config_ext_lib.get envs  in
    let target_without_ext = Filename.chop_suffix target ext_lib in
    let target_without_prefix = chop_prefix target_without_ext "lib" in
    let target = FileGen.add_basename a_file.file_dir.dir_file target_without_prefix in
    let cmd = new_command (ocamlmklib_cmd.get envs)
      [S "-custom"; S "-o"; F target] in
    List.iter (add_command_string cmd)
      (mklib_option.get lib.lib_opk.opk_options );
    List.iter (fun o_file ->
      add_command_arg cmd (BF o_file)) o_files;
    let r = new_rule lib a_file
      [Execute cmd] in
    add_more_rule_sources lib r [ ocamlmklib_deps ] envs;
    add_rule_sources r o_files;
    ()

let add_c_source b lib ptmp c_file options =
  let envs = options :: lib.lib_opk.opk_options in
  let dst_dir = lib.lib.lib_dst_dir in

  let basename = c_file.file_basename in
  let kernel_name = Filename.chop_suffix basename ".c" in
  let ext_obj  = BuildOCamlConfig.ocaml_config_ext_obj.get envs  in
  let o_file = add_dst_file lib dst_dir (kernel_name ^ ext_obj) in
  if not lib.lib_opk.opk_installed then
    add_c2o_rule b lib [] c_file o_file envs;
  ptmp.o_files := o_file :: !(ptmp.o_files)

let add_command_pack_args cmd modnames =
  if modnames <> [] then
    add_command_args cmd [S "-for-pack";
                          S (String.concat "." modnames)]


let move_compilation_garbage r copy_dir temp_dir kernel_name lib =


  let move_to_sources dst_dir_virt exts =
    let dst_dir = dst_dir_virt.dir_file in
    List.iter (fun ext ->
      let basename = kernel_name ^ ext in
      let src_file = FileGen.add_basename temp_dir basename in
      let dst_file = FileGen.add_basename dst_dir basename in
      let _maybe_file = add_file lib lib.lib.lib_mut_dir basename in
      add_rule_command r (MoveIfExists (F src_file, F dst_file, None))
    ) exts
  in
  move_to_sources lib.lib.lib_mut_dir [ ".annot"; ".s" ];

  let move_to_build exts =
    List.iter (fun ext ->
      let basename = kernel_name ^ ext in
      let src_file = FileGen.add_basename temp_dir basename in
      let dst_file = add_file lib lib.lib.lib_dst_dir basename in
      let link_file = add_file lib copy_dir (basename ^ ".link") in
      add_rule_command r (MoveIfExists
                            (F src_file, BF dst_file, Some (BF link_file)))
    ) exts
  in
  move_to_build [ ".cmt"; ".cmti"; ".spit"; ".spot"; ]



let do_copy_objects_from lib src_lib kernel_name extension obj_files =
(*
  Printf.eprintf "do_copy_objects_from %s:%s.%s -> %s:%s.%s\n%!"
    src_lib.lib_name kernel_name extension lib.lib_name kernel_name extension;
*)
  let obj_basename = kernel_name ^ extension in
  let obj_file_to_build =
    try
      let obj_file = find_dst_file lib.lib.lib_dst_dir obj_basename in
      if obj_file.file_target_of = [] then Some obj_file else None
    with NoSuchFileInDir _ ->
      Some (add_dst_file lib lib.lib.lib_dst_dir obj_basename)
  in
    match obj_file_to_build with
    | None -> ()
    | Some dst_obj_file ->
      try
        let src_obj_file = find_dst_file src_lib.lib.lib_dst_dir obj_basename in

        let r = new_rule lib dst_obj_file [] in
        add_rule_command r (Copy (BF src_obj_file, BF dst_obj_file));
        add_rule_source r src_obj_file;

        obj_files := dst_obj_file :: !obj_files
      with NoSuchFileInDir _ ->
        Printf.eprintf "Error: %s:%s is supposed to be copied from %s:%s that does not exist\n%!"
          lib.lib.lib_name obj_basename src_lib.lib.lib_name obj_basename;
        clean_exit 2


let get_copy_objects_from lib envs =
  match BuildValue.get_string_option_with_default envs "copy_objects_from" None with
    None -> None
  | Some name ->
    let bc = lib.lib.lib_builder_context in
    try
      BuildOCamlGlobals.get_by_id (StringMap.find name bc.packages_by_name)
    with Not_found ->
      Printf.eprintf "Error: in package %S, copy_objects_from %S, no such package\n%!" lib.lib.lib_name name;
      clean_exit 2

let copy_ml_objects_from lib ptmp src_lib kernel_name =
  (* TODO: check that pack_for = [] *)
  (* TODO: check that src_lib is in requires *)
  do_copy_objects_from lib src_lib kernel_name ".cmi" ptmp.cmi_files;
  if lib.lib_opk.opk_has_byte  then
    do_copy_objects_from lib src_lib kernel_name ".cmo" ptmp.cmo_files;
  if lib.lib_opk.opk_has_asm then begin
    do_copy_objects_from lib src_lib kernel_name ".cmx" ptmp.cmx_files;
    do_copy_objects_from lib src_lib kernel_name ".o" ptmp.cmx_o_files;
  end

let object_dst_dir b lib pack_for =
  let dst_dir = lib.lib.lib_dst_dir in
  match pack_for with
    [] -> dst_dir
  | modnames ->
    let name = String.concat "/" modnames in
    let full_dirname = Filename.concat dst_dir.dir_fullname name in
    if not lib.lib_opk.opk_installed then
      safe_mkdir full_dirname;
    BuildEngineContext.add_directory b full_dirname

let ml2odoc lib ptmp kernel_name envs before_cmd pack_for force temp_ml_file ml_file seq_order =
  if needs_odoc lib then
    let b = lib.lib.lib_context in
    let dst_dir = object_dst_dir b lib pack_for in

    let odoc_basename = kernel_name ^ ".odoc" in
    let odoc_file = add_dst_file lib dst_dir odoc_basename in

    let cmd = new_command (ocamldoc_cmd.get envs ) (docflags envs) in
    let r = new_rule lib odoc_file before_cmd in
    add_more_rule_sources lib r [ ocamldoc_deps ] envs;
    add_command_args cmd [S "-dump"; T odoc_basename];
    add_command_strings cmd (command_includes lib pack_for);
    if force = Force_IMPL || ml_file_option.get envs  then
      add_command_string cmd "-impl";
    add_command_arg cmd temp_ml_file;
    add_rule_command r (Execute cmd);
    add_rule_source r ml_file;
    cross_move r [ T odoc_basename, BF odoc_file ];
    ptmp.odoc_files := odoc_file :: !(ptmp.odoc_files);
    add_rule_sources r seq_order;
    ()

let mli2odoc lib ptmp kernel_name envs pack_for force mli_file seq_order =
  if needs_odoc lib then
    let b = lib.lib.lib_context in
    let dst_dir = object_dst_dir b lib pack_for in

    let odoc_basename = kernel_name ^ ".odoc" in
    let odoc_file = add_dst_file lib dst_dir odoc_basename in

    let cmd = new_command (ocamldoc_cmd.get envs ) (docflags envs) in
    let r = new_rule lib odoc_file [] in
    add_more_rule_sources lib r [ ocamldoc_deps ] envs;
    add_command_args cmd [S "-dump"; BF odoc_file];
    add_command_strings cmd (command_includes lib pack_for);
    if force = Force_INTF || mli_file_option.get envs  then
      add_command_string cmd "-intf";
    add_command_args cmd [ BF mli_file];
    add_rule_command r (Execute cmd);
    add_rule_source r mli_file;
    ptmp.odoc_files := odoc_file :: !(ptmp.odoc_files);
    add_rule_sources r seq_order;
    ()


let add_mli_source w b lib ptmp mli_file options =
  let envs = options :: lib.lib_opk.opk_options in


  if lib.lib_opk.opk_installed then () else
    let _ = () in

    let basename = mli_file.file_basename in
    let kernel_name = Filename.chop_extension basename in

    let copy_objects_from = get_copy_objects_from lib envs  in
    match copy_objects_from with
    | Some src_lib ->
      (* TODO: check that pack_for = [] *)
      (* TODO: check that src_lib is in requires *)
      do_copy_objects_from lib src_lib kernel_name ".cmi" ptmp.cmi_files;
    (* TODO: do the same for .odoc files ! *)

    | None ->
      if IntMap.mem mli_file.file_id ptmp.src_files then begin
        Printf.eprintf "Error: interface %s should be specified before its implementation in project %s\n%!"
          (file_filename mli_file) lib.lib.lib_name;
        clean_exit 2
      end;

      let pack_for = BuildValue.get_strings_with_default envs "packed" []  in
      let dst_dir = object_dst_dir b lib pack_for in

      ptmp.src_files <- IntMap.add mli_file.file_id mli_file ptmp.src_files;

      let copy_dir = copy_dir lib mli_file in
      let ppv = BuildOCamlSyntaxes.get_pp "mli" w lib basename options in
      let comp_deps = comp_deps w lib options in
      let mli_file, force =
        match ppv.pp_option with
          [] -> mli_file, Force_not
        | pp ->
          (* TODO: we should create the new_ml_file in the same subdirectory
             as the source file, not at the toplevel !! *)

          let new_mli_file =
            add_file lib lib.lib.lib_mut_dir (mli_file.file_basename ^ "pp")
          in

          let cmd = new_command pp (ppv.pp_flags @ [ BF mli_file ])  in
          add_command_pipe cmd (FileGen.to_string new_mli_file.file_file);

          let r = new_rule lib new_mli_file [] in
          add_more_rule_sources lib r [] envs;
          add_rule_command r (Execute cmd);
          BuildOCamlSyntaxes.add_pp_requires r ppv;
          add_more_rule_sources lib r [ pp_deps ] envs;
          add_rule_source r mli_file;
          add_rule_sources r comp_deps;
          new_mli_file, Force_INTF
      in

      let mldep_file =
        add_dst_file lib dst_dir (kernel_name ^ ".mlimods")
      in
      let needs_odoc = needs_odoc lib in
      let mldep_file_ok =
        add_ml2mldep_rule lib dst_dir pack_for force mli_file mldep_file needs_odoc options in
      let seq_order = [mldep_file_ok] in

      let cmi_basename = kernel_name ^ ".cmi" in
      let cmi_temp = add_temp_file lib  mli_file.file_dir cmi_basename in
      let cmi_file = add_dst_file lib dst_dir cmi_basename in

      let cmd, cmd_deps =
        if lib.lib_opk.opk_has_byte  then
          let cmd = new_command (ocamlc_cmd.get envs ) (bytecompflags envs) in
          add_bin_annot_argument cmd envs;
          add_command_args cmd [S "-c"; S "-o"; BF cmi_temp];
          add_command_strings cmd (command_includes lib pack_for);
          (*      add_command_strings cmd (command_pp lib options); *)
          if force = Force_INTF || mli_file_option.get envs  then
            add_command_args cmd [S "-intf" ];
          add_command_args cmd [BF mli_file];
          cmd, ocamlc_deps
        else
          let cmd = new_command (ocamlopt_cmd.get envs ) (asmcompflags envs) in
          add_bin_annot_argument cmd envs;
          add_command_args cmd [S "-c"; S "-o"; BF cmi_temp];
          add_command_strings cmd (command_includes lib pack_for);
          add_command_pack_args cmd pack_for;
          (*    add_command_strings cmd (command_pp lib options); *)
          if force = Force_INTF || mli_file_option.get envs  then
            add_command_string cmd  "-intf" ;
          add_command_args cmd [BF mli_file];
          cmd, ocamlopt_deps
      in
      let r = new_rule  lib cmi_file [Execute cmd] in
      add_more_rule_sources lib r [cmd_deps] envs;
      add_rule_sources r comp_deps;

      if cmi_temp != cmi_file then begin
        cross_move r [ BF cmi_temp, BF cmi_file ];
        add_rule_temporary r cmi_temp;
      end;
      move_compilation_garbage r copy_dir mli_file.file_dir.dir_file kernel_name lib;
      add_rule_source r mli_file;
      add_rule_sources r seq_order;

      (* TODO: we should actually rename all modules to fit
         their capitalized name in the _obuild directory *)
      let lib_modules =
        let pack_for = List.rev pack_for in
        try
          let (_, map) = StringsMap.find pack_for lib.lib_internal_modules in
          map
        with Not_found ->
          let map = ref StringMap.empty in
          lib.lib_internal_modules <-
            StringsMap.add pack_for (dst_dir, map) lib.lib_internal_modules;
          map
      in

      begin
        let (_is_ml, modname, basename) =
          BuildOCamldep.modname_of_file envs force mli_file.file_basename in
        try
          let (kind, basename) = StringMap.find modname !lib_modules in
          match kind with
            MLI -> ()
          | MLandMLI -> ()
          | ML ->
            lib_modules := StringMap.add modname (MLandMLI, basename) !lib_modules
        with Not_found ->
          if verbose 5 then
            Printf.eprintf "Adding module %s to %s\n" modname lib.lib.lib_name;
          lib_modules := StringMap.add modname (MLI, basename) !lib_modules
      end;

      mli2odoc lib ptmp kernel_name envs pack_for force mli_file seq_order;

      if pack_for = [] then
        ptmp.cmi_files := cmi_file :: !(ptmp.cmi_files)


let rec find_capital s len =
  if len > 0 then
    let pos = len-1 in
    let c = s.[pos] in
    if c = '/' || c = '\\' then len
    else
      find_capital s pos
  else 0

let invert_capital s =
  let len = String.length s in
  let pos = find_capital s len in
  (*  Printf.eprintf "invert_capital %S at pos %d\n%!" s pos; *)
  if pos < len then
    let s= Bytes.of_string s in
    let c = Bytes.get s pos in
    begin
      match c with
      | 'a'..'z' -> s.[pos] <- Char.uppercase c
      | 'A'..'Z' -> s.[pos] <- Char.lowercase c
      | _ -> ()
    end;
    Bytes.to_string s
  else s

let rec find_source_with_extension b lib src_dir kernel_name exts =
  match exts with
  | [] ->
    Printf.eprintf "Error: package %S, module %S, could not find\n"
      lib.lib.lib_name kernel_name;
    Printf.eprintf "   matching source in source directory\n";
    Printf.eprintf "   %S\n%!" src_dir.dir_fullname;
    clean_exit 2
  | ext :: rem_exts ->
    let basename1 = kernel_name ^ "." ^ ext in
    let test1 = FileGen.add_basename src_dir.dir_file basename1 in
    if FileGen.exists test1 then
      (basename1, ext)
    else
    let basename2 = invert_capital (kernel_name ^ "." ^ ext) in
    let test2 = FileGen.add_basename src_dir.dir_file basename2 in
    if FileGen.exists test2 then
      (basename2, ext)
    else
      find_source_with_extension b lib src_dir kernel_name rem_exts

let standard_source_exts =        [ "mly"; "mll"; "ml"; "mli"; "c" ]

let get_packed_objects lib r src_dir pack_of obj_ext =
  let options = lib.lib_opk.opk_options in
  let packed_cmx_files = ref [] in
  let b = r.rule_context in
  List.iter (fun basename ->
    let basename, extension = FileString.cut_at_last_extension basename in
    let (filename, _obj_extension) =
      if extension = "" then
        find_source_with_extension b lib src_dir basename
          [ obj_ext; "cmi" ]
      else
        let obj_extension = match String.lowercase extension with
       "ml" | "mll" | "mly" -> obj_ext
          | "mli" -> "cmi"
          | ext ->
            if List.mem ext (BuildValue.get_strings_with_default options
                  "impl_exts" []) then
              obj_ext
            else
            if List.mem ext (BuildValue.get_strings_with_default options
                  "intf_exts" []) then
              "cmi"
            else
            Printf.ksprintf failwith
              "Bad extension [%s] for filename [%s]" extension basename
        in
        (basename ^ "." ^  obj_extension, obj_extension)
    in
    let object_file = add_file lib src_dir filename in
    packed_cmx_files := object_file :: !packed_cmx_files;

    add_rule_source r object_file;
  ) pack_of;

  let packed_cmx_files = List.rev !packed_cmx_files in
  packed_cmx_files

let bprintf_list b name list =
  Printf.bprintf b "let %s = [\n" name;
  List.iter (fun s -> Printf.bprintf b "   %S;\n" s) list;
  Printf.bprintf b "   ]\n"


let (//) = Filename.concat

let rec find_git_commit dir =
  let git_dir = dir // ".git" in
  if Sys.file_exists git_dir then
    let filename = git_dir // "HEAD" in
    try
      let ref =
        let ic = open_in filename in
        let line = input_line ic in
        close_in ic;
        line
      in
      let ref, file = OcpString.cut_at ref ' ' in
      if ref = "ref:" then
        let ic = open_in (git_dir // file) in
        let line = input_line ic in
        close_in ic;
        line
      else ref

    with _ ->
      let head = try FileString.string_of_file filename with _ -> "??" in
      Printf.eprintf "Warning: unreadable-git-commit\nHEAD %S:\n%S\n%!"
        filename head;
      "unreadable-git-commit"
  else
    let new_dir = Filename.dirname dir in
    if dir = new_dir then "no-git-commit"
    else find_git_commit new_dir


let add_info b lib options name =
  match name with
  | "ocp::dates" ->
    let (date, en_date) =
      try
        BuildValue.get_string options "ocp_date",
        BuildValue.get_string options "ocp_en_date"
(*        ignore (Sys.getenv "OCPBUILD_NODATE");
          "NODATE", "NODATE (option OCPBUILD_NODATE)" *)
      with _ ->
    (* Tue Jan 20 17:48:12 CET 2015 *)
        let tm = MinUnix.localtime (MinUnix.time()) in
        let date =
          Printf.sprintf "%04d-%02d-%02d %02d:%02d"
            (1900+tm.MinUnix.tm_year) (1+tm.MinUnix.tm_mon)
            tm.MinUnix.tm_mday tm.MinUnix.tm_hour tm.MinUnix.tm_min
        in
        let en_date =
          try
            let date = MinUnix.strftime "%a %b %d %T %Z %Y" tm in
            if date = "" then failwith "strftime";
            date
          with _ -> date
        in
        (date, en_date)
    in
    Printf.bprintf b "let date = %S\n" date;
    Printf.bprintf b "let en_date = %S\n" en_date;

  | "ocp::commit" ->
    Printf.bprintf b "let commit = %S\n"
      (let commit = find_git_commit lib.lib.lib_src_dir.dir_fullname in
       try String.sub commit 0 8
       with _ -> commit
      )

  | name ->
    Printf.bprintf b "let %s = %S\n" name
      (BuildValue.get_string_with_default options name "")


let create_ml_file_if_needed lib mut_dir options ml_file =
  if BuildValue.get_bool_with_default options "ocp2ml" false then begin

    let tmp_ml = add_file lib mut_dir ml_file.file_basename in
    let tmp_ml_file = tmp_ml.file_file in
(* generate file in a buffer *)
    let b = Buffer.create 1000 in
    let opk = lib.lib_opk in
    Printf.bprintf b "(* Generated by ocp-build *)\n";
    Printf.bprintf b "let package = %S\n" lib.lib.lib_name;
    Printf.bprintf b "let version = %S\n" opk.opk_version;

    bprintf_list b "authors"
      (
        (BuildValue.get_strings_with_default options "author" [])
          @
        (BuildValue.get_strings_with_default options "authors" [])
      );
    List.iter (add_info b lib options) [
      "copyright";
      "license";
      "description";
    ];

    List.iter (fun variable ->
      bprintf_list b variable ( BuildValue.get_strings_with_default options variable [] )
    ) (BuildValue.get_strings_with_default options "env_lists" []);

    List.iter (add_info b lib options)
      (BuildValue.get_strings_with_default options "env_strings" []);

    List.iter (fun variable ->
      Printf.bprintf b "let %s = %b\n" variable
        (BuildValue.get_bool_with_default options variable false)
    ) (BuildValue.get_strings_with_default options "env_bools" []);

    Printf.bprintf b "let requires = [\n";
    List.iter (fun dep ->
      let lib = dep.dep_project in
      Printf.bprintf b "   %S, %S;\n" lib.lib.lib_name
        lib.lib_opk.opk_version;
    ) lib.lib_requires;
    Printf.bprintf b "  ]\n";

    let ml_content = Buffer.contents b in

    BuildEngineReport.cmd_file_from_content
      (FileGen.to_string tmp_ml_file) ml_content;

    if FileGen.exists tmp_ml_file then begin
      let old_ml_content = FileGen.read_file tmp_ml_file in
      if ml_content <> old_ml_content then begin
     if verbose 2 then
       Printf.fprintf stderr "create %s [outdated]\n%!"
         (FileGen.to_string tmp_ml_file);
            FileGen.write_file tmp_ml_file ml_content
      end
    end else begin
      if verbose 2 then
     Printf.fprintf stderr "create %s [unexisting] \n%!"
       (FileGen.to_string tmp_ml_file);
        FileGen.write_file tmp_ml_file ml_content;
    end;
    tmp_ml
  end else ml_file

(* Instead of copy_mli_if_needed that copies the mli file during
   OCamlBuildRules, we should instead create a rule to generate this
   file, and makes the .ml rules depend on it.
*)

let copy_mli_if_needed lib mut_dir mll_file kernel_name =
  try
    let mli_file = FileGen.add_basename mll_file.file_dir.dir_file (kernel_name ^ ".mli") in
    if FileGen.exists mli_file  then begin
      let mli_content = FileGen.read_file mli_file in
      let tmp_mli = add_file lib mut_dir (kernel_name ^ ".mli") in
      let tmp_mli_file = tmp_mli.file_file in
      BuildEngineReport.cmd_copy (FileGen.to_string mli_file)
        (FileGen.to_string tmp_mli_file);
      if FileGen.exists tmp_mli_file then
        let old_mli_content = FileGen.read_file tmp_mli_file in
        if mli_content <> old_mli_content then begin
          if verbose 2 then
            Printf.fprintf stderr "cp %s %s [outdated]\n%!"
              (FileGen.to_string mli_file) (FileGen.to_string tmp_mli_file);
          FileGen.write_file tmp_mli_file mli_content
        end else
          ()
      else begin
        if verbose 2 then
          Printf.fprintf stderr "cp %s %s [unexisting] \n%!"
            (FileGen.to_string mli_file) (FileGen.to_string tmp_mli_file);
        FileGen.write_file tmp_mli_file mli_content;
      end
    end (* else
           Printf.eprintf "MLI FILE %S does not exist\n%!"
           (FileGen.to_string mli_file); *)

  with e ->
    Printf.eprintf "copy_mli_if_needed error %s\n%!" (Printexc.to_string e);
    clean_exit 2

(* Shall we infer the presence of the mli file ? We should probably ask the user
   to tell the build system that the mli does not exist. *)


let add_ml_source w b lib ptmp ml_file options =
  let needs_odoc = needs_odoc lib in

  let envs = options :: lib.lib_opk.opk_options in
  let basename = ml_file.file_basename in
  (*  Printf.eprintf "basename = [%s]\n" basename; *)
  let kernel_name =
    BuildValue.get_string_with_default envs "module"
      (Filename.chop_extension basename) in

  let has_byte = lib.lib_opk.opk_has_byte in
  let has_asm = lib.lib_opk.opk_has_asm in


  let orig_ml_file = ml_file in
  let pack_for = BuildValue.get_strings_with_default envs "packed" []  in
  if lib.lib_opk.opk_installed then begin

    if pack_for = [] then begin

      (*
        Printf.eprintf "add_ml_source: %s is already installed in %s\n%!"
        basename (FileGen.to_string dst_dir.dir_file);
        Printf.eprintf "ml_file %s\n%!" (file_filename ml_file);
      *)

      let dst_dir = ml_file.file_dir in

      let cmo_basename = kernel_name ^ ".cmo" in
      let cmo_file = add_dst_file lib dst_dir cmo_basename in

      let cmx_basename = kernel_name ^ ".cmx" in
      let cmx_file = add_dst_file lib dst_dir cmx_basename in

      let ext_obj  = BuildOCamlConfig.ocaml_config_ext_obj.get envs  in
      let o_basename = kernel_name ^ ext_obj in
      let o_file = add_dst_file lib dst_dir o_basename in

      (* TODO: we should check that they do exist !! *)
      if has_byte then
        ptmp.cmo_files := cmo_file :: !(ptmp.cmo_files);
      if has_asm then begin
        ptmp.cmx_files := cmx_file :: !(ptmp.cmx_files);
        ptmp.cmx_o_files := o_file :: !(ptmp.cmx_o_files)
      end
    end

  end else

    let comp_deps = comp_deps w lib options in
    let copy_objects_from = get_copy_objects_from lib envs  in
    match copy_objects_from with
    | Some src_lib ->
      copy_ml_objects_from lib ptmp src_lib kernel_name

    | None ->

      let copy_dir = copy_dir lib ml_file in
      let old_ml_file = ml_file in
      let ml_file = create_ml_file_if_needed lib lib.lib.lib_mut_dir envs ml_file in
      let ppv = BuildOCamlSyntaxes.get_pp "ml" w lib basename options in

      (* [has_mli] = None | Some (build_file, in_source_directory_predicate) *)
      let _has_mli =
        if no_mli_option.get envs then None else
          try
            let mli_file = BuildValue.get_string envs mli_file_attr in
            Some (add_package_file lib mli_file, false)
          with Var_not_found _ ->
            let mli_name = kernel_name ^ ".mli" in
            let mli_file =
              Filename.concat
                orig_ml_file.file_dir.dir_fullname
                mli_name
            in
            (* do that before pp_option change it ! *)
            if Sys.file_exists mli_file then
              Some (add_file lib orig_ml_file.file_dir mli_name, true)
            else
              try
                Some (find_dst_file lib.lib.lib_src_dir (kernel_name ^ ".mli"), true)
              with NoSuchFileInDir _ -> None
      in

      let ml_file =
        let file2string = BuildValue.get_strings_with_default envs
           file2string_attr []  in
        if file2string = [] then ml_file else
          let new_ml_file = add_file lib lib.lib.lib_mut_dir ml_file.file_basename
          in
          let r = new_rule lib new_ml_file [] in
          (* TODO: for bytecode, we should generate the .mli too *)
          let sources = List.map (fun file ->
            file, add_file lib orig_ml_file.file_dir file
          ) file2string in
          add_rule_sources r (List.map snd sources);
          add_rule_command r (
            Function ("file2string",
                      (fun b ->
                        List.iter (fun (file, _) ->
                          Printf.bprintf b "%s\n" file
                        ) sources
                      ),
                      (fun () ->
                        let b = Buffer.create 10000 in
                        Printf.bprintf b "let files = [\n";
                        List.iter (fun (file, src_file) ->
                          Printf.bprintf b "%S, %S;"
                            file (FileString.string_of_file (file_filename src_file))
                        ) sources;
                        Printf.bprintf b "  ]\n";
                        let content = Buffer.contents b in
                        let file = file_filename new_ml_file in
                        BuildEngineReport.cmd_file_from_content file content;
                        FileString.file_of_string file content;
                        ())));
          new_ml_file
      in

      let ml_file, force =
        match ppv.pp_option with
          [] -> ml_file, Force_not
        | pp ->
          (* TODO: we should create the new_ml_file in the same subdirectory
             as the source file, not at the toplevel !! *)

          let new_ml_file =
            add_file lib lib.lib.lib_mut_dir (ml_file.file_basename ^ "pp")
          in

          let cmd = new_command pp (ppv.pp_flags @ [ BF ml_file ])  in
          add_command_pipe cmd (FileGen.to_string new_ml_file.file_file);

          let r = new_rule lib new_ml_file [] in
          add_more_rule_sources lib r [] envs;

          add_rule_command r (Execute cmd);
          BuildOCamlSyntaxes.add_pp_requires r ppv;
          add_more_rule_sources lib r [ pp_deps ] envs;
          add_rule_source r ml_file;
          add_rule_sources r comp_deps;
          new_ml_file, Force_IMPL
      in

      if old_ml_file != ml_file then begin
        (* Why is javascript files rebuilt ?
        Printf.eprintf "Need to copy mli file for %S\n%!"
          (file_filename old_ml_file);
        *)
          copy_mli_if_needed lib lib.lib.lib_mut_dir old_ml_file kernel_name;
      end;


      let dst_dir = object_dst_dir b lib pack_for in
      let pack_of = pack_option.get envs  in

      (*
        if pack_of <> [] then
        List.iter (fun pack -> Printf.eprintf "pack %s\n" pack) pack_of;
      *)


      let modname = String.capitalize kernel_name in

      let cmi_name = kernel_name ^ ".cmi" in

      (* TODO: we already check for this previously in _has_mli. Why not use
         it ? *)
      let needs_cmi =
        try
          (* This case corresponds to a .mli file present in "files"
             before the .ml *)
          let cmi_file = find_dst_file dst_dir cmi_name in
          Some cmi_file
        with NoSuchFileInDir _ ->
          let mli_name = kernel_name ^ ".mli" in
          let mli_file =
            Filename.concat
              orig_ml_file.file_dir.dir_fullname
              mli_name
          in
          if not (no_mli_option.get  envs ) then
            (* do that before pp_option change it ! *)
            let mli_file =
              if Sys.file_exists mli_file then
                Some (add_file lib orig_ml_file.file_dir mli_name)
              else
                try
                  Some (find_dst_file lib.lib.lib_src_dir (kernel_name ^ ".mli"))
                with NoSuchFileInDir _ -> None
            in
            match mli_file with
            | Some mli_file ->
         (* MLI file does exist !!! We should probably put a warning, as we
            have no information on how to compile this file !!*)

              ignore (add_mli_source w b lib ptmp mli_file (BuildValue.set_bool options "ml" false) : unit);
              let cmi_file = find_dst_file dst_dir cmi_name in
              Some cmi_file
            | None -> None
          else
            None
      in

      let seq_order =
        if pack_of <> [] then
          [] (* don't compute dependencies when we already know them *)
        else
          let mldep_file =
            add_dst_file lib dst_dir (kernel_name ^ ".mlmods")
          in
          ptmp.src_files <- IntMap.add ml_file.file_id ml_file ptmp.src_files;
          let mldep_file_ok = add_ml2mldep_rule lib dst_dir pack_for force ml_file mldep_file
            (needs_odoc && needs_cmi = None) options in
          ptmp.dep_files <- IntMap.add mldep_file.file_id mldep_file ptmp.dep_files;
          [mldep_file_ok]
      in
      let seq_order = match needs_cmi with
          None -> seq_order
        | Some cmi_file -> cmi_file :: seq_order in
      let gen_cmi = match needs_cmi with
          None -> [add_dst_file lib dst_dir cmi_name ]
        | Some _ -> []
      in

      let lib_modules =
          let pack_for = List.rev pack_for in
          try
            let (_, map) = StringsMap.find pack_for lib.lib_internal_modules in
            map
          with Not_found ->
            let map = ref StringMap.empty in
            lib.lib_internal_modules <- StringsMap.add pack_for (dst_dir, map) lib.lib_internal_modules;
            map
      in

      begin
        let (_is_ml, modname, basename) = BuildOCamldep.modname_of_file envs force ml_file.file_basename in
        try
          let (kind, basename) =  StringMap.find modname !lib_modules in
          let error filename =
            Printf.eprintf
              "ERROR: The file(s) %s appears more than once in %s\n%!"
              filename
              lib.lib.lib_filename in
          match kind with
            ML       -> error (basename ^ ".ml")
          | MLandMLI -> error (basename ^ ".ml and " ^ basename ^ ".mli")
          | MLI ->
            lib_modules := StringMap.add modname (MLandMLI, basename) !lib_modules
        with Not_found ->
          if verbose 5 then
            Printf.eprintf "Adding module %s to %s\n" modname lib.lib.lib_name;
          lib_modules := StringMap.add modname (ML, basename) !lib_modules
      end;


      let cmi_basename = kernel_name ^ ".cmi" in
      let cmi_file = add_dst_file lib dst_dir cmi_basename in

      let (before_cmd, temp_ml_file) =
        if no_mli_option.get envs  then
          let temp_ml_file = T (kernel_name ^ ".ml") in
          ([ NeedTempDir; Copy (BF ml_file, temp_ml_file)], temp_ml_file)
        else
          ([], BF ml_file)
      in

      let needs_cmo =
        if has_byte then  begin

          let cmo_basename = kernel_name ^ ".cmo" in
          let cmo_file = add_dst_file lib dst_dir cmo_basename in

          let cmd = new_command (ocamlc_cmd.get envs ) (bytecompflags envs) in
          let r = new_rule lib cmo_file before_cmd in
          add_more_rule_sources lib r [ ocamlc_deps ] envs;
          add_rule_sources r comp_deps;

          (*    let temp_dir = BuildEngineRules.rule_temp_dir r in
                let cmo_temp = FileGen.add_basename temp_dir cmo_basename in
                let cmi_temp = FileGen.add_basename temp_dir cmi_basename in *)

          add_bin_annot_argument cmd envs;


          if pack_of = [] then begin
            add_command_args cmd [S "-c"; S "-o"; T cmo_basename];
            add_command_pack_args cmd pack_for;
            add_command_strings cmd (command_includes lib pack_for);
            (*      add_command_strings cmd (command_pp ptmp options); *)
            if force = Force_IMPL || ml_file_option.get envs  then
              add_command_string cmd "-impl";
            add_command_arg cmd temp_ml_file;

            add_rule_command r (Execute cmd);
            add_rule_source r ml_file;
          end else begin
            add_command_args cmd [S "-pack"; S "-o"; T cmo_basename];
            add_command_pack_args cmd pack_for;

            let src_dir = Filename.concat dst_dir.dir_fullname modname in
            (*      Printf.eprintf "Pack in %s [%s]\n" src_dir modname; *)
            let src_dir = BuildEngineContext.add_directory b src_dir in
            let cmo_files = get_packed_objects lib r src_dir pack_of "cmo" in
            let cmd = add_files_to_link_to_command lib "byte pack" cmd envs cmo_files in
            add_rule_command r cmd
          end;

          cross_move r [ T cmo_basename, BF cmo_file ];
          begin match needs_cmi with
            None ->
              cross_update r [T cmi_basename, BF cmi_file]
          | _ -> ();
          end;

          if pack_for = [] then
            ptmp.cmo_files := cmo_file :: !(ptmp.cmo_files);


          move_compilation_garbage r copy_dir
            (BuildEngineRules.rule_temp_dir r) kernel_name lib;

          add_rule_sources r seq_order;
          add_rule_targets r gen_cmi;
          match needs_cmi with
            None -> Some cmo_file
          | Some _ -> None
        end else None
      in

      let _needs_cmx =
        if has_asm then begin

          let cmx_basename = kernel_name ^ ".cmx" in
          let cmx_file = add_dst_file lib dst_dir cmx_basename in

          let ext_obj  = BuildOCamlConfig.ocaml_config_ext_obj.get envs  in
          let o_basename = kernel_name ^ ext_obj in
          let o_file = add_dst_file lib dst_dir o_basename in

          let cmd = new_command (ocamlopt_cmd.get envs ) (asmcompflags envs) in
          let r = new_rule  lib cmx_file before_cmd in
          add_more_rule_sources lib r [ ocamlopt_deps] envs;
          add_rule_sources r comp_deps;
          add_bin_annot_argument cmd envs;
          (*
            let temp_dir = BuildEngineRules.rule_temp_dir r in
            let o_temp = FileGen.add_basename temp_dir o_basename in
            let cmx_temp = FileGen.add_basename temp_dir cmx_basename in
            let cmi_temp = FileGen.add_basename temp_dir cmi_basename in
          *)

          if pack_of = [] then begin
            add_command_args cmd [S "-c"; S "-o"; T cmx_basename];
            add_command_pack_args cmd pack_for;
            add_command_strings cmd (command_includes lib pack_for);
            (*      add_command_strings cmd (command_pp ptmp options); *)
            if force = Force_IMPL ||  ml_file_option.get envs  then
              add_command_string cmd "-impl" ;
            add_command_arg cmd temp_ml_file;

            add_rule_command r (Execute cmd);
            add_rule_source r ml_file;
          end else begin
            add_command_args cmd [S "-pack"; S "-o"; T cmx_basename];
            add_command_pack_args cmd pack_for;

            let src_dir = BuildEngineContext.add_directory b (Filename.concat dst_dir.dir_fullname modname) in
            let cmx_files = get_packed_objects lib r src_dir pack_of "cmx" in
            let cmd = add_files_to_link_to_command lib "asm pack" cmd envs cmx_files in
            add_rule_command r cmd
          end;

          cross_move r [ T cmx_basename, BF cmx_file;
                         T o_basename, BF o_file ];
          begin match needs_cmi with
            None ->
              cross_update r [T cmi_basename, BF cmi_file]
          | _ -> ();
          end;
          add_rule_sources r seq_order;
          add_rule_targets r (o_file :: gen_cmi);
          move_compilation_garbage r copy_dir (BuildEngineRules.rule_temp_dir r) kernel_name lib;

          begin match needs_cmo with
            Some cmo_file ->
         (* If both ocamlc and ocamlopt build the cmi file, they should
            not execute concurrently. For that, we create an artificial
            ordering between them, by requesting the cmo file before
            the cmx file, if both have to be generated. *)

         (* TODO: is this still useful ? Now that we build in a
            temporary directory, there is no need for that, no ? *)
              add_rule_time_dependency r cmo_file
          | None -> ()
          end;

          if pack_for = [] then begin
            ptmp.cmx_files := cmx_file :: !(ptmp.cmx_files);
            ptmp.cmx_o_files := o_file :: !(ptmp.cmx_o_files);
          end;
          Some cmx_file
        end else None
      in

      begin
        match needs_cmi with
        | Some _ -> ()
        | None ->
          if pack_of = [] then
            ml2odoc lib ptmp kernel_name envs before_cmd pack_for force temp_ml_file ml_file seq_order
      end;

      if pack_for = [] then begin
        if needs_cmi = None then
          ptmp.cmi_files := cmi_file :: !(ptmp.cmi_files);

      end

let add_mll_source w b lib ptmp mll_file options =
  let envs = options :: lib.lib_opk.opk_options in
  let basename = mll_file.file_basename in
  let kernel_name = Filename.chop_suffix basename ".mll" in

  if lib.lib_opk.opk_installed then

    let ml_file = add_file lib lib.lib.lib_src_dir (kernel_name ^ ".ml") in
    add_ml_source w b lib ptmp ml_file options

  else

    let copy_objects_from = get_copy_objects_from lib envs  in
    match copy_objects_from with
    | Some src_lib ->
      copy_ml_objects_from lib ptmp src_lib kernel_name
    | None ->

(*    let tmp_dirname =
      Filename.concat
        (Filename.concat b.build_dir_filename "_temp_tree")
        (FileGen.to_string mll_file.file_dir.dir_file) in
    if not (Sys.file_exists tmp_dirname) then safe_mkdir tmp_dirname;
    let tmp_dir = add_directory b tmp_dirname in *)
(*    let copy_dir = copy_dir lib mll_file in *)
    let _ = () in
    copy_mli_if_needed lib lib.lib.lib_mut_dir mll_file kernel_name;


    let ml_file = add_file lib lib.lib.lib_mut_dir (kernel_name ^ ".ml") in
    add_mll2ml_rule lib mll_file ml_file options;
    add_ml_source w b lib ptmp ml_file options

let add_mly_source w b lib ptmp mly_file options =
  let envs = options :: lib.lib_opk.opk_options in
  let basename = mly_file.file_basename in
  let kernel_name = Filename.chop_suffix basename ".mly" in

  if lib.lib_opk.opk_installed then
    let ml_file = add_file lib mly_file.file_dir (kernel_name ^ ".ml") in
    add_ml_source w b lib ptmp ml_file options
  else

    let copy_objects_from = get_copy_objects_from lib envs  in
    match copy_objects_from with
    | Some src_lib ->
      copy_ml_objects_from lib ptmp src_lib kernel_name
    | None ->

      let _ = () in
      (*    let copy_dir = copy_dir lib mly_file in *)

      let ml_file = add_file lib lib.lib.lib_mut_dir (kernel_name ^ ".ml") in
      let mli_filename = kernel_name ^ ".mli" in
      let mli_file = add_file lib lib.lib.lib_mut_dir mli_filename in
      add_mli_source w b lib ptmp mli_file options;
      add_mly2ml_rule lib mly_file ml_file mli_file options;
      add_ml_source w b lib ptmp ml_file options

let process_source w b lib ptmp src_dir (basename, options) =
  let _bc = lib.lib.lib_builder_context in
  let envs = options :: lib.lib_opk.opk_options in

  let (kernel_name, last_extension) = OcpString.rcut_at basename '.' in
  let (basename, last_extension) =
    if last_extension = "" then
      find_source_with_extension b lib src_dir kernel_name
        standard_source_exts
    else
      (basename, last_extension)
  in
  let src_file = try
                   add_file lib src_dir basename
    with MinUnix.Unix_error(MinUnix.ENOENT, _, _) ->
      (* This actually only happens when the source file is located in a non-existing directory *)
      Printf.eprintf "Error: missing source file %S for package %S\n%!"
        (Filename.concat src_dir.dir_fullname basename) lib.lib.lib_name;
      Printf.eprintf "  (You may need to  manually disable compilation of this package\n";
      Printf.eprintf "  with 'enabled = false')\n%!";
      clean_exit 2

  in
  match last_extension with
    "c" ->
      add_c_source b lib ptmp src_file options

        (*
  | "objects" ->
    let obj_lib =
      try
        StringMap.find kernel_name bc.packages_by_name
      with Not_found ->
        Printf.eprintf "Package %s: Could not find %s.objects in:\n%!"
          lib.lib.lib_name kernel_name;
        StringMap.iter (fun s _ -> Printf.eprintf "%s " s) bc.packages_by_name;
        Printf.eprintf "\n%!";
        clean_exit 2
    in
    begin match BuildOCamlGlobals.get_by_id obj_lib with
    | None -> ()
    | Some obj_lib ->
      ptmp.cmo_files := (List.rev obj_lib.lib_cmo_objects) @ !(ptmp.cmo_files);
      ptmp.cmx_files := (List.rev obj_lib.lib_cmx_objects) @ !(ptmp.cmx_files);
      ptmp.cmx_o_files := (List.rev obj_lib.lib_cmx_o_objects) @ !(ptmp.cmx_o_files);
    ()
    end

  | "files" ->
    let obj_lib =
      try
        StringMap.find kernel_name bc.packages_by_name
      with Not_found ->
        Printf.eprintf "Package %s: Could not find %s.objects\n%!"
          lib.lib.lib_name kernel_name;
        clean_exit 2
    in
    begin match BuildOCamlGlobals.get_by_id obj_lib with
    | None -> ()
    | Some obj_lib ->
      let src_dir = obj_lib.lib.lib_src_dir in
      List.iter (process_source w b lib ptmp src_dir) obj_lib.lib_sources
    end
        *)

  | "ml" ->
    add_ml_source w b lib ptmp src_file options
  | "mll" ->
    add_mll_source w b lib ptmp src_file options
  | "mly" ->
    add_mly_source w b lib ptmp src_file options
  | "mli" ->
    add_mli_source w b lib ptmp src_file options
    (* other ones: .ml4, mli4, .ml5, .mli5, .mly4, .mly5, .mll4, .mll5 *)
  | ext ->
    if ml_file_option.get envs
      || List.mem ext (BuildValue.get_strings_with_default envs "ml_exts" [])
      || List.mem ext (BuildValue.get_strings_with_default envs "impl_exts" [])
    then
      add_ml_source w b lib ptmp src_file options
    else
      if mli_file_option.get envs
        || List.mem ext (BuildValue.get_strings_with_default envs "mli_exts" [])
        || List.mem ext (BuildValue.get_strings_with_default envs "intf_exts" [])
      then
        add_mli_source w b lib ptmp src_file options
      else
        if
          List.mem ext (BuildValue.get_strings_with_default envs "mll_exts" [])
        then
          add_mll_source w b lib ptmp src_file options
        else
          if
            List.mem ext (BuildValue.get_strings_with_default envs "mly_exts" [])
          then
            add_mly_source w b lib ptmp src_file options
          else
            begin

              Printf.eprintf "Don't know what to do with [%s] (extension %S)\n%!"
                (String.escaped basename) ext;
              Printf.eprintf "\tfrom project %s in dir %s\n%!"
                lib.lib.lib_name src_dir.dir_fullname;
              clean_exit 2;
            end

let process_source w b lib ptmp src_dir (basename, options) =
  let bc = lib.lib.lib_builder_context in
  let envs = options :: lib.lib_opk.opk_options in
  let src_dir =
    let package = package_option.get  envs  in
    if package = "" then src_dir else
      let obj_lib =
        try
          StringMap.find package bc.packages_by_name
        with Not_found ->
          Printf.eprintf "Package %s: Could not find package %s\n%!"
            lib.lib.lib_name package;
          clean_exit 2
      in
      let src_dir = obj_lib.lib_src_dir in
      src_dir
  in
  let basename =
    let subdir = subdir_option.get envs  in
    match subdir with
        [] -> basename
      | subdir ->
(* Since basename can be a relative filename, we use both
  FileGen.t and strings. Clearly, it is not good, and we should
  convert basename to FileGen.t earlier *)
        let subdir = FileGen.add_basenames (FileGen.of_string "") subdir in
        Filename.concat (FileGen.to_string subdir) basename
  in
  process_source w b lib ptmp src_dir (basename, options)

let process_sources w b lib =
  let ptmp = new_package_temp_variables () in
  begin
    match lib.lib.lib_type with
    | SyntaxPackage ->
      if lib.lib_sources <> [] then begin
        Printf.eprintf "Syntax %S: 'files' should be empty !\n" lib.lib.lib_name;
        Printf.eprintf "   If your syntax contains sources, you should build a library\n";
        Printf.eprintf "   and define the syntax to require this library.\n%!";
        clean_exit 2
      end
    | RulesPackage -> assert false
    | TestPackage
    | LibraryPackage
    | ProgramPackage

    | ObjectsPackage ->
      let src_dir = lib.lib.lib_src_dir in
      let _dst_dir = lib.lib.lib_dst_dir in
      List.iter (process_source w b lib ptmp src_dir) lib.lib_sources;
  end;

  ptmp.cmo_files := List.rev !(ptmp.cmo_files);
  ptmp.odoc_files := List.rev !(ptmp.odoc_files);
  lib.lib_doc_targets := !(ptmp.odoc_files) @ !(lib.lib_doc_targets);
  ptmp.cmx_files := List.rev !(ptmp.cmx_files);
  ptmp.cmx_o_files := List.rev !(ptmp.cmx_o_files);
  ptmp.cmi_files := List.rev !(ptmp.cmi_files);
  ptmp.o_files := List.rev !(ptmp.o_files);
  ptmp

let add_library w b lib =
  (*  let src_dir = lib.lib.lib_src_dir in *)
  let dst_dir = lib.lib.lib_dst_dir in
  let envs = lib.lib_opk.opk_options in
  let ptmp = process_sources w b lib in


  let cclib =  cclib_option.get envs in
  let cclib = String.concat " " cclib in
  let (cclib, stubs_files) =
    let a_file =
      let ext_lib = BuildOCamlConfig.ocaml_config_ext_lib.get envs  in
      let libbasename =
        Printf.sprintf "lib%s%s" lib.lib_stubarchive ext_lib in
      if !(ptmp.o_files) <> [] then
        let a_file = add_dst_file lib dst_dir libbasename in
        add_os2a_rule lib !(ptmp.o_files) a_file;
        Some a_file
      else
        try
          let a_file = libstubs.get envs in
          if a_file = "" then raise (Var_not_found "libstubs");
          let a_file = BuildSubst.subst_global a_file in
          if Filename.basename a_file <> libbasename then begin
            Printf.eprintf "%s\nError: %s=%S basename differs from %S^%s^%S=\"%s\"\n%!"
              (string_of_libloc lib)
              "libstubs" a_file "lib" "stubarchive" ext_lib libbasename;
            BuildMisc.clean_exit 2
          end;
          let a_file = add_package_file lib a_file in
          Some a_file
        with Var_not_found _ -> None
    in
    match a_file with
    | None -> cclib, []
    | Some a_file ->
      lib.lib_stub_targets <- (a_file, STUB_A) :: lib.lib_stub_targets;
      Printf.sprintf "-l%s %s" lib.lib_stubarchive cclib, [a_file]

  in

  if  lib.lib_opk.opk_has_byte &&
    (lib.lib_opk.opk_installed || !(ptmp.cmo_files) <> []) then begin
      let cma_file = add_dst_file lib dst_dir (lib.lib_archive ^ ".cma") in
      add_cmo2cma_rule lib ptmp cclib !(ptmp.cmo_files) cma_file;
      lib.lib_intf_targets <-
        (List.map (fun cmi -> cmi, CMI)
           (!(ptmp.cmi_files))) @ lib.lib_intf_targets;
      lib.lib_byte_targets <- (cma_file, CMA) :: lib.lib_byte_targets;
    end;

  if  lib.lib_opk.opk_has_asm &&
    (lib.lib_opk.opk_installed || !(ptmp.cmx_files) <> []) then begin
      let (cmxa_file, a_file, cmxs_files) =
        add_cmx2cmxa_rule lib cclib !(ptmp.cmi_files)
          !(ptmp.cmx_files) !(ptmp.cmx_o_files) stubs_files in
      lib.lib_intf_targets <-
        (List.map (fun cmi -> cmi, CMI) (!(ptmp.cmi_files))) @
        (List.map (fun cmx -> cmx, CMX) (!(ptmp.cmx_files))) @
        lib.lib_intf_targets;
      lib.lib_asm_targets <-
        (cmxa_file, CMXA) ::
        (a_file, CMXA_A) ::
        cmxs_files @ lib.lib_asm_targets
    end;

  if !(ptmp.odoc_files) <> [] then begin
    let doc_dirname = Filename.concat dst_dir.dir_fullname "_doc" in
    safe_mkdir doc_dirname;
    let docdir = BuildEngineContext.add_directory b doc_dirname in
    let html_file = add_file lib dst_dir "_doc/index.html" in
    add_odocs2html_rule lib !(ptmp.odoc_files) docdir html_file;
    lib.lib_doc_targets := html_file :: !(lib.lib_doc_targets)
  end;
  ()


let add_objects w b lib =
  let ptmp = process_sources w b lib in

  if lib.lib_opk.opk_has_byte then begin
    lib.lib_intf_targets <-
      (List.map (fun cmi -> cmi, CMI) (!(ptmp.cmi_files))) @
      lib.lib_intf_targets;
    lib.lib_byte_targets <-
        (List.map (fun cmo -> cmo, CMO)
           (!(ptmp.cmo_files))) @ lib.lib_byte_targets;
  end;
  if lib.lib_opk.opk_has_asm then begin
    lib.lib_intf_targets <-
      (List.map  (fun cmi -> cmi, CMI) (!(ptmp.cmi_files))) @
      (List.map  (fun cmx -> cmx, CMX) (!(ptmp.cmx_files))) @
      lib.lib_intf_targets;
    lib.lib_asm_targets <-
      (List.map (fun cmx -> cmx, CMX)
         (!(ptmp.cmx_files)))
    @ (List.map (fun o -> o, CMX_O)
         (!(ptmp.cmx_o_files)))
    @ lib.lib_asm_targets;
  end;
  ()

let local_subst (file, env) s =
  let s = BuildSubst.subst_global s in
  let s = BuildSubst.apply_substituter
      BuildOCP.filesubst s (file,env) in
  s

let add_extra_rules bc lib target_name target_files =
  let lib_options = lib.lib_opk.opk_options in
  let _b = bc.build_context in
  let dirname = lib.lib.lib_dirname in
  let files = BuildValue.get_strings_with_default lib_options "source_files" [] in
  List.iter (fun file ->
    let (_: build_file) = add_file lib lib.lib.lib_src_dir file
    in
    ()
  ) files;
  let build_rules =
    BuildValue.get_local_prop_list_with_default lib_options
      (target_name ^ "_rules") [] in
  let build_targets =
    BuildValue.get_local_prop_list_with_default lib_options
      (target_name ^ "_targets") [] in

  List.iter (fun (file, _env) ->
      let file = BuildSubst.subst_global file in
      let target_file = add_package_file lib file in
      target_files := target_file :: !target_files
  ) build_targets;

  if build_rules <> [] then
    List.iter (fun (file, env) ->
(*
      Printf.eprintf "Adding rule to build %s/%s\n%!" (FileGen.to_string dirname) file;
*)

      let envs = env :: lib.lib_opk.opk_options in

      let uniq_rule = BuildValue.get_string_option_with_default envs "uniq_rule" None in

      let file = BuildSubst.subst_global file in
      let target_file = add_package_file lib file in

      let to_build = BuildValue.get_bool_with_default envs "build_target" false in
      if to_build then
        target_files := target_file :: ! target_files;

      try
        match uniq_rule with
        None -> raise Not_found
        | Some uniq_rule ->
          let r = Hashtbl.find bc.uniq_rules uniq_rule in
          add_rule_target r target_file
      with Not_found ->

(*      let substituted_words = BuildValue.get_strings_with_default envs "subst" [] in *)
      let local_subst = local_subst (file, envs) in
      let targets = BuildValue.get_strings_with_default envs "more_targets" [] in
      let targets = List.map local_subst targets in

      let commands =
        try
          BuildValue.get_local_prop_list envs "commands"
        with Var_not_found _ ->
          Printf.eprintf "Error in package %S at %S:\n%!"
                         lib.lib.lib_name
                         (BuildEngineDisplay.string_of_loc lib.lib.lib_loc);
          Printf.eprintf "\tRule for %S does not define 'commands'\n%!" file;
          clean_exit 2
      in
      let sources = BuildValue.get_strings_with_default envs "sources" [] in
      let sources = List.map local_subst sources in


      let r = new_rule lib target_file [] in
      begin match uniq_rule with
        None -> () | Some uniq_rule ->
          Hashtbl.add bc.uniq_rules uniq_rule r
      end;

      let sources = List.map (add_package_file lib) sources in
      let dirname_s = FileGen.to_string dirname in

      List.iter (fun (cmd_name, cmd_env) ->
        let envs = cmd_env :: envs in
        match cmd_name with
        | "" ->
          let cmd =
            try
              let cmd = BuildValue.get_strings envs "value" in
              cmd
            with Var_not_found _ -> assert false
          in
          let cmd = List.map local_subst cmd in
          let cmd = new_command cmd [] in
          begin
            let dirname_s = try
              let s = BuildValue.get_string envs "chdir" in
              let s = local_subst s in
              if Filename.is_relative s then
                Filename.concat dirname_s s
              else s
            with Var_not_found _ -> dirname_s
            in
            cmd.cmd_move_to_dir <- Some dirname_s
          end;
          let get_pipe name =
            try
              let stdout = BuildValue.get_string envs name in
              let stdout = local_subst stdout in
              let stdout = if Filename.is_relative stdout then
                  Filename.concat dirname_s stdout
                else stdout
              in
              Some stdout
            with Var_not_found _ -> None
          in

          cmd.cmd_stdin_pipe <- get_pipe "stdin";
          cmd.cmd_stdout_pipe <- get_pipe "stdout";
          cmd.cmd_stderr_pipe <- get_pipe "stderr";

          add_rule_command r (Execute cmd)

        | "%%loaddeps" ->

          make_virtual_file target_file;

          let loader filename =
            let dependencies =
              BuildOCamldep.load_make_dependencies filename
            in
            List.map (fun (file, deps) ->
              (Filename.concat dirname_s file,
               List.map (fun file -> [
                 if Filename.is_relative file then
                   Filename.concat dirname_s file
                 else file ]) deps)
            ) dependencies
          in
          List.iter (fun source_file ->
            add_rule_command r
              (LoadDeps (loader, source_file, r))
          ) sources;

          r.rule_forced <- true;
          (* must be executed, even when no changes *)

        | "%%subst" ->
          let to_file = BuildValue.get_path_with_default envs "to_file" file in
          let to_file = local_subst to_file in
          let to_file = add_package_file lib to_file in
          let from_file = BuildValue.get_path_with_default envs "from_file" (file ^ ".in") in
          let from_file = local_subst from_file in
          let from_file = add_package_file lib from_file in
          let substitutions = BuildValue.prop_list (BuildValue.get envs "substitutions") in

          let substitutions =
            List.map (fun  (string, string_env)  ->
              let envs = string_env :: envs in
              let with_string =
                try
                  local_subst (BuildValue.get_string envs "with_string")
                with Var_not_found _ ->
                  failwith (Printf.sprintf "In command %%subst, string %s has no 'with_string'" string)
              in
              (string, with_string)
            ) substitutions in
          let subst = List.fold_left (fun subst (string, with_string) ->
              StringMap.add string with_string subst
            ) StringMap.empty substitutions
          in

          let printer b =
            Printf.bprintf b "subst %S %S\n"
              (file_filename from_file) (file_filename to_file);
            List.iter (fun (string, with_string) ->
              Printf.bprintf b "\t%S -> %S\n" string with_string
            ) substitutions;
          in
          let actor () =
            let s = FileString.string_of_file (file_filename from_file) in
            let s = BuildSubst.map_subst subst s in
            FileString.file_of_string (file_filename to_file) s
          in
          add_rule_source r from_file;
          add_rule_target r to_file;
          add_rule_command r (Function (cmd_name, printer, actor))

        | "%%config_make2ocp" ->
          let to_file = BuildValue.get_path_with_default envs "dst" file in
          let from_file = BuildValue.get_path envs "src" in

          let from_file = local_subst from_file in
          let to_file = local_subst to_file in

          let from_file = add_package_file lib from_file in
          let to_file = add_package_file lib to_file in

          let printer b =
            Printf.bprintf b "config_make2ocp %S -> %S\n"
              (file_filename from_file) (file_filename to_file)
          in
          let actor () =
            Printf.eprintf "Loading %S\n" (file_filename from_file);
            let make_subst = OcpSubst.empty_subst () in
            OcpSubst.add_to_subst make_subst "\\ " " ";
            let vars = ref [] in
            FileGen.iter_lines (fun line ->
              let _, line = OcpSubst.iter_subst make_subst line in
              if String.length line > 0 && line.[0] <> '#' then
                let var, value = OcpString.cut_at line '=' in
                OcpSubst.add_to_subst make_subst
                  (Printf.sprintf "$(%s)" var) value;
                vars := (var, value) :: !vars
            ) from_file.file_file;
            let vars = List.rev !vars in
            Printf.eprintf "Writing %S\n" (file_filename to_file);
            let oc = open_out (file_filename to_file) in
            List.iter (fun (var, value) ->
              Printf.fprintf oc "%s = %S\n" var value
            )  vars;
            close_out oc;
            ()
          in
          add_rule_source r from_file;
          add_rule_target r to_file;
          add_rule_command r (Function (cmd_name, printer, actor));
        | _ ->
          Printf.eprintf "Error: Unknown primitive command %S in %s\n" cmd_name
            (BuildEngineDisplay.string_of_loc lib.lib.lib_loc);
          Printf.eprintf "  Commands to execute should be between { ... }, while\n";
          Printf.eprintf "  primitive commands start by %% (for example %%loaddeps)\n%!";
          clean_exit 2
      ) commands;

      add_more_rule_sources lib r [] envs;

      add_rule_sources r sources;

      let targets = List.map (add_package_file lib) targets in
      add_rule_targets r targets;

    ) build_rules;
  ()


let add_program w b lib =
  let lib_options =  lib.lib_opk.opk_options in
  let dst_dir = lib.lib.lib_dst_dir in
  let ptmp = process_sources w b lib in

  begin (* Fast check of libraries modules *)
    let map = ref StringMap.empty in
    List.iter (fun dep ->
      if dep.dep_link then
        let lib1 = dep.dep_project in
          match lib1.lib.lib_type with
          | TestPackage -> assert false
          | ProgramPackage
        (*        | ProjectToplevel *)
          | ObjectsPackage
            -> ()
          | RulesPackage
          | LibraryPackage ->
            StringsMap.iter (fun _ (_, modules) ->
              StringMap.iter (fun modname (kind1, _) ->
                try
                  let (kind2, lib2) = StringMap.find modname !map in
                  match kind1, kind2 with
                  | (ML | MLandMLI), (ML | MLandMLI) ->
                    Printf.eprintf
                      "Warning: program %s, requirements %s and %s\n"
                      lib.lib.lib_name lib2.lib.lib_name lib1.lib.lib_name;
                    Printf.eprintf "\tboth define a module name %s.\n" modname;
                  | _ -> ()
                with Not_found ->
                  map := StringMap.add modname (kind1,lib1) !map
              ) !modules
            ) lib1.lib_internal_modules
          | SyntaxPackage ->
            (* Nothing to do ? *)
            ()
    ) lib.lib_requires
  end;

  let cclib =  cclib_option.get lib_options in
  let cclib = String.concat " "  cclib in
  let is_toplevel = is_toplevel.get lib_options in
  let linkall = force_link_option.get lib_options || is_toplevel in
  begin
    let linkflags = bytelinkflags lib in
    let linkflags =
      if linkall || !(ptmp.cmo_files) = [] then
        S "-linkall" :: linkflags
      else linkflags
    in
    let byte_file = add_dst_file lib dst_dir (lib.lib_archive ^ byte_exe) in
    add_cmo2byte_rule lib ptmp linkflags cclib !(ptmp.cmo_files)
      !(ptmp.o_files) byte_file;
    if lib.lib_opk.opk_has_byte  then begin
      lib.lib_byte_targets <- (byte_file, RUN_BYTE) :: lib.lib_byte_targets;
    end
  end;

  if !(ptmp.cmx_files) <> [] then begin
    let linkflags = asmlinkflags lib in
    let linkflags =
      if linkall || !(ptmp.cmx_files) = [] then  S "-linkall" :: linkflags
      else linkflags in
    let asm_file = add_dst_file lib dst_dir (lib.lib_archive ^ asm_exe) in
    add_cmx2asm_rule lib ptmp linkflags cclib
      !(ptmp.cmx_files) !(ptmp.cmx_o_files) !(ptmp.o_files) asm_file;
    if  lib.lib_opk.opk_has_asm && not is_toplevel then begin
      lib.lib_asm_targets <- (asm_file, RUN_ASM) :: lib.lib_asm_targets;
    end
  end;
  ()

let fix_windows_directory s =
  let s = Bytes.of_string s in
  let len = Bytes.length s in
  for i = 0 to len - 1 do
    if Bytes.get s i = '\\' then s.[i] <- '/'
  done;
  let rec iter i =
    if i = 0 then "." else
    if Bytes.get s (i-1) = '/' then iter (i-1)
    else
      if i = len
      then Bytes.to_string s
      else Bytes.sub_string s 0 i
  in
  iter len

let add_package bc opk =
  let pk = opk.opk_package in
  let b = bc.build_context in
  let package_name = pk.BuildOCPTypes.package_name in
  let package_dirname = pk.BuildOCPTypes.package_dirname in
  let package_options = opk.opk_options in
  try
    if verbose 7 then Printf.eprintf "Adding %s\n" package_name;

    let package_dirname =
      try
        let list = BuildValue.strings_of_plist ( BuildValue.get_local package_options "dirname" ) in
        BuildSubst.subst_global (String.concat Filename.dir_sep list)
      with Var_not_found _ ->
        package_dirname
    in

    let package_dirname = fix_windows_directory package_dirname in

    if verbose 7 then Printf.eprintf "\tfrom %s\n" package_dirname;
    let src_dir = BuildEngineContext.add_directory b (absolute_filename package_dirname) in
    if verbose 7 then Printf.eprintf "\tfrom %s\n" src_dir.dir_fullname;

    let already_installed = BuildValue.is_already_installed package_options
    in

    let dst_dir =
      if already_installed then src_dir else
        let dirname =
          Filename.concat b.build_dir_filename package_name
     (*     Filename.concat src_dir.dir_fullname build_dir_basename *)
        in
        safe_mkdir dirname;
        BuildEngineContext.add_directory b dirname
    in
    if verbose 7 then Printf.eprintf "\tto %s\n" dst_dir.dir_fullname;


    let mut_dir =
      if already_installed then src_dir else
        let mut_dirname =
          Filename.concat dst_dir.dir_fullname "_temp"
        in
        safe_mkdir mut_dirname;
        BuildEngineContext.add_directory b mut_dirname
    in

    let lib = BuildGlobals.new_library bc pk
      package_dirname src_dir dst_dir mut_dir in
    let lib = BuildOCamlGlobals.create_package lib opk in
    (* TOOD: we should do that in one pass before *)
    BuildSubst.add_to_global_subst
      (Printf.sprintf "%s_SRC_DIR" package_name) src_dir.dir_fullname;
    BuildSubst.add_to_global_subst
      (Printf.sprintf "%s_DST_DIR" package_name) dst_dir.dir_fullname;

    let full_src_dir = absolute_filename src_dir.dir_fullname in
    let full_dst_dir = absolute_filename dst_dir.dir_fullname in
    BuildSubst.add_to_global_subst
      (Printf.sprintf "%s_FULL_SRC_DIR" package_name)
      full_src_dir;
    BuildSubst.add_to_global_subst
      (Printf.sprintf "%s_FULL_DST_DIR" package_name)
      full_dst_dir;
    lib
  with Failure s ->
    Printf.eprintf "While preparing package %S:\n%!" package_name;
    Printf.eprintf "Error: %s\n%!" s;
    clean_exit 2

let plugin =
  let module Plugin = struct
    let name = "OCaml"
  end in
  (module Plugin : Plugin)

let create w cin cout bc state =

  BuildOCamlGlobals.reset ();
(*  BuildOCPPrinter.eprint_project "BuildOCamlRules.create" ptmp; *)
  let b = bc.build_context in
  let libs =
    Array.map (fun pk ->
      match pk.BuildOCPTypes.package_plugin with
      | OCamlPackage opk ->
        add_package bc opk
      | _ -> assert false
    ) state.BuildOCPTypes.project_sorted
  in
  Array.iter (fun lib ->
    try
      if not lib.lib_opk.opk_installed then
        safe_mkdir lib.lib.lib_dst_dir.dir_fullname;
      add_extra_rules bc lib "build" lib.lib_build_targets;
      add_extra_rules bc lib "doc" lib.lib_doc_targets;
      add_extra_rules bc lib "test" lib.lib_test_targets;

      lib.lib_linkdeps <- get_link_order lib;
      (*
      Printf.eprintf "linkdeps for %S : %s\n%!" lib.lib.lib_name
        (String.concat " "
           (List.map (fun lib -> lib.lib.lib_name) lib.lib_linkdeps));
      *)

      begin
        if not lib.lib_opk.opk_installed then
          match lib.lib.lib_type with
            LibraryPackage -> add_library w b  lib
          | ProgramPackage -> add_program w b  lib
          | TestPackage ->
            if lib.lib_sources <> [] then add_program w b  lib;
            lib.lib_opk.opk_install <- false;
          | ObjectsPackage -> add_objects w b  lib
          | SyntaxPackage -> ()
          | RulesPackage -> ()
      end;

      let options = lib.lib_opk.opk_options in

      let set_objects lib name kinds f =
        let objs = BuildValue.get_strings_with_default options name [] in
        if objs <> [] then
          f (
            List.flatten (
              List.map (fun s0 ->
            let s = BuildSubst.subst_global s0 in
            let bf = add_package_file lib s in
            let basename = bf.file_basename in
            match List.rev (OcpString.split basename '.') with
            | [] ->
              Printf.eprintf
                "Error: package %S, option %S contains a file %S\n"
                lib.lib.lib_name name s0;
              Printf.eprintf "  with no extension\n%!";
              exit 2
            | ext :: _ ->
              match ext with
              | "asm" when List.mem RUN_ASM kinds -> [ bf, RUN_ASM ]
              | "byte"  when List.mem RUN_BYTE kinds -> [ bf, RUN_BYTE ]
              | "cmxs" when List.mem CMXS kinds -> [ bf, CMXS ]
              | "cmx" when List.mem CMX kinds ->
                let s = Filename.chop_extension s ^ ".o" in
                let bf2 = add_package_file lib s in
                [ bf, CMX; bf2, CMX_O ]
              | "cmxa" ->
                let s = Filename.chop_extension s ^ ".a" in
                let bf2 = add_package_file lib s in
                [ bf, CMXA; bf2, CMXA_A ]
              | "cmi" when List.mem CMI kinds -> [ bf, CMI ]
              | "cmo" when List.mem CMO kinds -> [ bf, CMO ]
              | "cma" when List.mem CMA kinds -> [ bf, CMA ]
              | "o" when List.mem STUB_A kinds -> [ bf, STUB_A ]
              | "a" when List.mem STUB_A kinds -> [ bf, STUB_A ]
              | ext ->
                Printf.eprintf
                  "Error: package %S, option %S contains a file %S\n"
                  lib.lib.lib_name name s0;
                Printf.eprintf "  with unexpected extension %S\n%!" ext;
                exit 2
              ) objs))
      in

      set_objects lib "intf_targets" [CMI; CMX]
        (fun objs -> lib.lib_intf_targets <- objs);

      set_objects lib "byte_targets" [CMO;CMA;RUN_BYTE]
        (fun objs ->
          lib.lib_byte_targets <- objs);

      set_objects lib "asm_targets" [CMX;CMXA;CMXS;RUN_ASM]
        (fun objs ->
          lib.lib_asm_targets <- objs);

      set_objects lib "stub_targets" [STUB_A]
        (fun objs -> lib.lib_stub_targets <- objs);

      begin
        try
          lib.lib_modules <- [StringsMap.find [] lib.lib_internal_modules]
        with Not_found ->
          let objs =
            let objs = BuildValue.get_strings_with_default
              options "internal_targets" [] in
            if objs = [] then
              BuildValue.get_strings_with_default
                options "intf_targets" []
            else []
          in
          let dirs = ref [] in
          List.iter (fun s0 ->
             let s = BuildSubst.subst_global s0 in
             let bf = add_package_file lib s in
             let dst_dir = bf.file_dir in
             let (is_ml, modname, basename) =
               BuildOCamldep.modname_of_file options Force_not
                 bf.file_basename in
             let map =
               try
                 List.assq dst_dir !dirs
               with Not_found ->
                 let map = ref StringMap.empty in
                 dirs := (dst_dir, map) :: !dirs;
                 map
             in
             try
             match StringMap.find modname !map with
             | (ML, _) when not is_ml ->
               map := StringMap.add modname (MLandMLI, basename) !map
             | (MLI, _) when is_ml ->
               map := StringMap.add modname (MLandMLI, basename) !map
             | (MLandMLI, _) -> ()
             | _ -> raise Not_found
             with Not_found ->
               map := StringMap.add modname (
                 (if is_ml then ML else MLI), basename) !map
          ) objs;
          lib.lib_modules <- !dirs
      end;

    with Failure s ->
      Printf.eprintf "While preparing package %S:\n%!" lib.lib.lib_name;
      Printf.eprintf "Error: %s\n%!" s;
      clean_exit 2
  ) libs;


  if !BuildOCamlGlobals.list_byte_targets_arg then begin
    Printf.eprintf "Bytecode targets:\n";
    StringMap.iter (fun _ lib ->
      match BuildOCamlGlobals.get_by_id lib with
      | None -> ()
      | Some lib ->
      if lib.lib_byte_targets <> [] then begin
        List.iter (fun (target, _kind) ->
          Printf.eprintf "\t%s\t->\t%s\n" lib.lib.lib_name target.file_basename)
          lib.lib_byte_targets;
      end) bc.packages_by_name;
    Printf.eprintf "%!"
  end;

  if !BuildOCamlGlobals.list_asm_targets_arg then begin
    Printf.eprintf "Native targets:\n";
    StringMap.iter (fun _ lib ->
      match BuildOCamlGlobals.get_by_id lib with
      | None -> ()
      | Some lib ->
      if lib.lib_asm_targets <> [] then begin
        List.iter (fun (target, _kind) ->
          Printf.eprintf "\t%s\t->\t%s\n" lib.lib.lib_name target.file_basename)
          lib.lib_asm_targets;
      end) bc.packages_by_name;
    Printf.eprintf "%!"
  end;

  let install_where = BuildOCamlInstall.install_where cin cout in
  let install_what = BuildOCamlInstall.install_what () in

  let pks =
  Array.map (fun lib ->
    let module P = struct
      let name = lib.lib.lib_name
      let info = lib.lib
      let plugin = plugin

      let clean_targets () = assert false
      let build_targets () =
        (*
        Printf.eprintf " (pk %s)\n" lib.lib_opk.opk_name;
        Printf.eprintf " (dir %s)\n" lib.lib_opk.opk_dirname;
         *)
        if lib.lib_opk.opk_installed then begin
          (*          Printf.eprintf "%s is already installed\n%!" name; *)
          {
          targets = [];
          depends = [];
          } end
        else
          let targets = BuildOCamlGlobals.make_build_targets lib.lib cin in
(*          Printf.eprintf " %s.build_targets =\n * %s\n End\n"
            name
            (String.concat "\n * "
            (List.map (fun f -> file_filename f) targets)); *)
        let depends =
          let depends = ref [] in
          List.iter (fun dep ->
            if dep.dep_link || dep.dep_syntax then
              depends := dep.dep_project.lib :: !depends
          ) lib.lib_requires;
          !depends
        in
        { targets; depends }
      let test_targets () =
        let targets = BuildOCamlGlobals.make_test_targets lib.lib cin in
        let depends =
          let depends = ref [] in
          List.iter (fun dep ->
            if dep.dep_link || dep.dep_syntax then
              depends := dep.dep_project.lib :: !depends
          ) lib.lib_requires;
          !depends
        in
        { targets; depends }
      let doc_targets () =
        let targets = BuildOCamlGlobals.make_doc_targets lib.lib cin in
        let depends =
          let depends = ref [] in
          List.iter (fun dep ->
            if dep.dep_link || dep.dep_syntax then
              depends := dep.dep_project.lib :: !depends
          ) lib.lib_requires;
          !depends
        in
        { targets; depends }
      let conf_targets () =
        let targets = BuildOCamlGlobals.make_build_targets lib.lib cin in
        let depends =
          let depends = ref [] in
          List.iter (fun dep ->
            if dep.dep_link || dep.dep_syntax then
              depends := dep.dep_project.lib :: !depends
          ) lib.lib_requires;
          !depends
        in
        { targets; depends }

      (* lazy because shared AND it can creates directories *)
      let install_dir =
        lazy (BuildOCamlInstall.find_installdir
                install_where lib)

      (* where to look for previously installed packages *)
      let install_dirs () = install_where.install_libdirs

      let test () = assert false

      (* TODO *)
      let install_dir () =
        match Lazy.force install_dir with
        | None -> assert false
        | Some install_dir -> install_dir

      let pre_installed () = lib.lib_opk.opk_installed

      let to_install () = lib.lib_opk.opk_install

      let install () =
        if lib.lib_opk.opk_install then
          let installdir = install_dir () in
          BuildOCamlInstall.install
            install_where install_what
            lib.lib installdir


    end in
    (module P : BuildTypes.Package)
  ) libs
  in

  pks

let () =
  BuildOCamlOCP2.init ()
