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
open BuildOCamlTypes
open BuildOCamlVariables
open BuildEngineTypes
open BuildTypes
open BuildOCPTypes
open BuildValue.TYPES

let verbose = OcpDebug.verbose_function [ "B" ;"BuildOCamldep" ]

let load_ocamldep_modules filename =
  let ic = open_in filename in
  let s = input_line ic in
  close_in ic;
  let (source, modules) = OcpString.cut_at s ':' in
  let modules = OcpString.split modules ' ' in
  source, modules

(* With packing, tracking dependencies is trickier. From a given module,
we can only access internal modules of the current project, and external
modules of the other projects.
*)

let modname_of_file options force filename =
  let filename = Filename.basename filename in
  let is_ml =
    Filename.check_suffix filename ".ml"
    || Filename.check_suffix filename ".mll"
    || Filename.check_suffix filename ".mly"
    || Filename.check_suffix filename ".cmx"
    || Filename.check_suffix filename ".cmo"
    || force = Force_IMPL
    || BuildValue.get_bool_with_default options "ml" false
  in
  let basename = Filename.chop_extension filename in
  let modname = String.capitalize basename in
  is_ml, modname, basename

let filter_deps options option modules =
  let nodeps =
    let nodeps = ref StringSet.empty in
    List.iter (fun modname ->
      nodeps := StringSet.add modname !nodeps)
      (option.get options);
    !nodeps
  in
  List.filter (fun modname ->
    not (StringSet.mem modname nodeps)) modules

let load_modules_dependencies lib options force dst_dir pack_for needs_odoc filename =
  let envs = options :: lib.lib_opk.opk_options in
  let has_asm = lib.lib_opk.opk_has_asm in
  let has_byte = lib.lib_opk.opk_has_byte in

  if verbose 5 then
    Printf.eprintf "load_modules_dependencies %s\n" filename;
  let source, modules = load_ocamldep_modules filename in
  if verbose 5 then begin
    Printf.eprintf "  source: %s\n" source;
    Printf.eprintf "  modules: %s\n" (String.concat " " modules);
  end;
  let modules =
    if nopervasives.get envs then modules
    else "Pervasives" :: modules
  in

  let (is_ml, _modname, _basename) = modname_of_file envs force source in

  let basename = Filename.chop_extension (Filename.basename filename) in

  let modules =
    if not is_ml || nointernaldeps.get envs then modules
    else
      "CamlinternalLazy" ::
        "CamlinternalOO" ::
        "CamlinternalMod" ::
        modules
  in

  let modules = filter_deps envs nodeps_option modules in
  let cmx_modules = filter_deps envs nocmxdeps_option modules in

  let deps = lib.lib_requires in
  let deps = List.concat (List.map (fun dep ->
    let olib = dep.dep_project in

    if dep.dep_link ||
      (BuildValue.get_bool_with_default
         [dep.dep_options] "externals_only" false) then
      olib.lib_modules
    else []
                            ) (List.rev deps)) in
  let deps = (lib.lib.lib_dst_dir, ref lib.lib_aliases) :: deps in
  let rec add_internal_deps pack_for deps =
    let deps =
      match pack_for with
      | [] -> deps
      | _ :: tail -> add_internal_deps tail deps in
    let map =
      try
        [StringsMap.find pack_for lib.lib_internal_modules]
      with Not_found ->
        if pack_for = [] then
          lib.lib_modules
        else begin
          Printf.eprintf
            "Internal error: missing internal module map for pack %S\n%!"
            (String.concat "." pack_for);
          exit 2
        end
    in
    map @ deps
  in
  let deps = add_internal_deps (List.rev pack_for) deps in

  if verbose 6  then begin
    Printf.eprintf "load_modules_dependencies path %s\n" filename;
    List.iter (fun (dst_dir, map) ->
      Printf.eprintf "   DIR: %S\n\t" dst_dir.dir_fullname;
      StringMap.iter (fun modname _ ->
        Printf.eprintf " %s " modname
      ) !map;
      Printf.eprintf "\n"
    ) deps;

  end;

  let enter_cmi_dep dependencies dst_dir kind basename =
    let dst_dir = dst_dir.dir_fullname in
    let full_basename = Filename.concat dst_dir basename in
    match kind with
    | ML ->
       (*
            let deps = [] in
            let deps = if has_asm then
                (full_basename ^ ".cmx") :: deps else deps
            in
            let deps = if has_byte then
                (full_basename ^ ".cmo") :: deps else deps
            in
              *)
       let deps = [full_basename ^ ".cmo"; full_basename ^ ".cmx" ] in
       dependencies := deps :: !dependencies
    | MLI ->
       dependencies := [ full_basename ^ ".cmi" ] :: !dependencies
    | MLandMLI ->
       dependencies := [ full_basename ^ ".cmi" ] :: !dependencies

  in

  let depends_only_on_cmi exts =
    let dependencies = ref [] in
    let rec find_module deps depname =
      (*      Printf.eprintf "find_module CMI %s\n" depname; *)
      match deps with
        [] ->
          if verbose 5 then
            Printf.eprintf "Warning: could not solve dependency %s for %s\n" depname filename;
          ()
      | (dst_dir, lib_modules) :: deps ->
        try
          let (kind, basename) = StringMap.find depname !lib_modules in
          match basename with
          | DepAlias alib ->
             if lib != alib then
             StringMap.iter (fun _ (kind, basename) ->
                 match basename with
                 | DepBasename basename ->
                    enter_cmi_dep dependencies dst_dir kind basename
                 | DepAlias _ -> assert false
               ) alib.lib_aliases
          | DepBasename basename ->
             enter_cmi_dep dependencies dst_dir kind basename
        with Not_found ->
          find_module deps depname
    in
    List.iter (find_module deps) modules;
    List.map (fun ext ->
      let target = Filename.concat dst_dir.dir_fullname (basename ^ ext) in
      target, !dependencies) exts
  in

  let dependencies =
    if is_ml then

      let byte_dependencies =
        if has_byte then

          let cmo_target = Filename.concat dst_dir.dir_fullname (basename ^ ".cmo") in

          let cmo_dependencies =
            let cmo_dependencies = ref [] in
            let rec find_module deps depname =
              (*      Printf.eprintf "find_module CMO %s\n" depname; *)
              match deps with
                [] ->
                  if verbose 5 then
                    Printf.eprintf
                      "Warning: could not solve dependency %s for %s\n"
                      depname filename;
                  ()
              | (dst_dir, lib_modules) :: deps ->
                try
                  let (kind, basename) = StringMap.find depname !lib_modules in
                  enter_cmo_dep dst_dir kind basename
                with Not_found ->
                  find_module deps depname
            and enter_cmo_dep dst_dir kind basename =
              match basename with
              | DepAlias alib ->
                 if lib != alib then
                 StringMap.iter (fun _ (kind,basename) ->
                     enter_cmo_dep dst_dir kind basename
                   ) alib.lib_aliases
              | DepBasename basename ->
                 let dst_dir = dst_dir.dir_fullname in
                 let full_basename = Filename.concat dst_dir basename in
                 let deps =
                   match kind with
                   | ML ->
                      [ full_basename ^ ".cmo" ]
                   | MLI ->
                      [ full_basename ^ ".cmi" ]
                   | MLandMLI ->
                      [ full_basename ^ ".cmi" ]
                 in
                 cmo_dependencies := deps :: !cmo_dependencies
            in
            List.iter (find_module deps) modules;
            !cmo_dependencies
          in
          [ cmo_target, cmo_dependencies ]
        else []
      in
      let asm_dependencies =
        if has_asm then
          let cmx_target =
            Filename.concat dst_dir.dir_fullname (basename ^ ".cmx") in
          (*          Printf.eprintf "cmx_target=%s\n%!" cmx_target; *)
          let cmx_dependencies =
            let cmx_dependencies = ref [] in
            let rec find_module deps depname =
              (* Printf.eprintf "  find_module CMX %s\n" depname; *)
              match deps with
                [] ->
                  if verbose 5 then
                    Printf.eprintf
                      "Warning: could not solve dependency %s for %s\n"
                      depname filename;
                  ()
              | (dst_dir, lib_modules) :: deps ->
                try
                  let (kind, basename) = StringMap.find depname !lib_modules in
                  (* Printf.eprintf "  Found in %s\n%!" dst_dir.dir_fullname; *)
                  enter_cmx_dep dst_dir kind basename
                with Not_found ->
                  find_module deps depname
            and enter_cmx_dep dst_dir kind basename =
              match basename with
              | DepAlias alib ->
                 if lib != alib then
                   StringMap.iter (fun _modname (kind, basename) ->
                       enter_cmx_dep dst_dir kind basename
                     ) alib.lib_aliases
              | DepBasename basename ->
                 let src_dir = dst_dir.dir_fullname in
                 let full_basename = Filename.concat src_dir basename in
                 let deps =
                   match kind with
                   | ML ->
                      [ full_basename ^ ".cmx" ]
                   | MLI ->
                      [ full_basename ^ ".cmi" ]
                   | MLandMLI ->
                      [ full_basename ^ ".cmx" ]
                 in
                 cmx_dependencies := deps :: !cmx_dependencies
            in
            List.iter (find_module deps) cmx_modules;
            !cmx_dependencies

          in
          [ cmx_target, cmx_dependencies ]
        else
          []
      in
      let dependencies =
        byte_dependencies @ asm_dependencies
      in
      if needs_odoc then
        (depends_only_on_cmi [ ".odoc"]) @ dependencies
      else dependencies
    else
      depends_only_on_cmi (if needs_odoc then [".odoc"; ".cmi"] else [".cmi"])
  in

  if verbose 5 then
    Printf.eprintf "load_modules_dependencies %s DONE\n" filename;
  if verbose 3 then
    BuildDepMisc.print_dependencies dependencies;
  dependencies
