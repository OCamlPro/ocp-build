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

open MetaTypes
open BuildTypes
open BuildEngineTypes
open BuildEngineGlobals

open BuildValue.TYPES
open BuildOCamlTypes
open BuildOCPTypes (* for dep_link *)
open BuildUninstall.TYPES

(* TODO:
  When installing, we should accept the option -sanitize, to clean everything
  before, i.e. remove META files that would be refer to the same package.
  We should check that there is nothing remaining before installing, and
  ask the user to use -sanitize if necessary to remove conflicting files.
*)


(*
   When to generate META files ?
   - 1st easy: when we install files into one of the ocamlfind directories
        The META file is in the directory, without "directory"
   - 2nd easy: when we install files into OCAMLLIB subdirectories
        The META. file is in one of the ocamlfind directories, and
        the "directory" starts with "^"
   - 3rd : when we install files into somewhere else
        The META. file is in one of the ocamlfind directory, and
        the "directory" is absolute.
*)

module TYPES = struct

type install_where = {
  install_destdir : string option;
  install_libdirs : string list;
  install_bindir : string;
  install_datadir : string option;

  install_ocamlfind : string list;
  install_ocamllib : string;
}

type install_what = {
  install_byte_bin : bool;
  install_asm_bin : bool;
  install_byte_lib : bool;
  install_asm_lib : bool;
}

end
open TYPES

module List = struct
  include List

  let rec split_after l1 l2 =
    match l1, l2 with
      _, [] -> Some l1
    | [], _ -> None
    | h1 :: t1, h2 :: t2 ->
      if h1 = h2 then
        split_after t1 t2
      else
        None
end


let split_dir dir =
  let rec iter pos pos0 path dir len =
    if pos = len then begin
      if pos = pos0 then List.rev path else
        List.rev (String.sub dir pos0 (pos-pos0) :: path)
    end else
      match dir.[pos] with
        '/' | '\\' ->
          let path =
            if pos = pos0 then path
            else
              String.sub dir pos0 (pos-pos0) :: path
          in
          let pos = pos+1 in
          iter pos pos path dir len
      | _ -> iter (pos+1) pos0 path dir len
  in
  iter 0 0 [] dir (String.length dir)

let in_destdir where file =
  match where.install_destdir with
    None -> file
  | Some destdir -> Filename.concat destdir file

let rec safe_mkdir where log filename =
  let filename_d = in_destdir where filename in
  try
    let st = MinUnix.stat filename_d in
(*    Printf.eprintf "safe_mkdir: %S exists\n%!" filename_d; *)
    match st.MinUnix.st_kind with
      MinUnix.S_DIR -> ()
    | _ ->
      failwith (Printf.sprintf
                  "FileGen.safe_mkdir: %S exists, but is not a directory"
                  filename)
  with MinUnix.Unix_error (MinUnix.ENOENT, _, _) ->
(*    Printf.eprintf "safe_mkdir: %S doesnot exist\n%!" filename_d; *)
    let dirname = Filename.dirname filename in
    safe_mkdir where log dirname;
    let basename = Filename.basename filename in
    match basename with
    | "." | ".." -> ()
    | _ ->
      MinUnix.mkdir filename_d 0o755;
      BuildUninstall.add_un_field log DIR filename

(* [dst] must be the target file name, not the name of its directory *)
let rec copy_rec where log src dst =
    (*    Printf.eprintf "copy_rec: %S -> %S\n%!" src dst; *)
  let dst_d = in_destdir where dst in
  let st = MinUnix.stat src in
  match st.MinUnix.st_kind with
  | MinUnix.S_DIR ->
    safe_mkdir where log dst;
    FileString.iter_dir (fun basename ->
      copy_rec where log (Filename.concat src basename)
        (Filename.concat dst basename)) src
  | MinUnix.S_REG ->
    BuildUninstall.add_un_field log FILE dst;
    FileString.copy_file src dst_d;
    MinUnix.chmod dst_d st.MinUnix.st_perm
  | _ ->
    failwith (Printf.sprintf
                "FileGen.copy_rec: cannot copy unknown kind file %S"
                src)

let copy_file where log src_file dst_file =
  Printf.eprintf " %s%!" (Filename.basename dst_file);
  copy_rec where log src_file dst_file


(* TODO: we should do an analysis on the packages that are going to be
  installed, to check that all libraries have also their dependencies
  loaded. *)

let install_META log where installdir meta lib =

  if BuildOCamlVariables.install_META.get lib.lib_opk.opk_options then
    let really_install_META meta_file =
      let meta_file_d = in_destdir where meta_file in
      safe_mkdir where log (Filename.dirname meta_file);
      MetaFile.file_of_meta meta_file_d meta;
      BuildUninstall.add_un_field log FILE meta_file;
      Printf.eprintf "Generated META file %s\n%!" meta_file;
    in

  (* What kind of META file do we create ? *)
    let topdir_list = split_dir (Filename.dirname installdir) in
    let ocamlfind_path = List.map split_dir where.install_ocamlfind in

    if List.mem topdir_list ocamlfind_path then
      really_install_META (Filename.concat installdir "META")
    else
      let meta_basename = Printf.sprintf "META.%s" lib.lib.lib_name in
      let installdir_list = split_dir installdir in
      let rec iter dirs =
        match dirs with
        | dir :: dirs ->
          begin
            let dir_list = split_dir dir in
            match List.split_after installdir_list dir_list with
            | None -> iter dirs
            | Some subdir ->
                MetaFile.set_directory meta  (String.concat "/" subdir);
              really_install_META (Filename.concat dir meta_basename)
          end
        | [] ->
          begin
            let ocamllib = split_dir where.install_ocamllib in
            MetaFile.set_directory meta
              (match List.split_after installdir_list ocamllib with
              | None -> installdir
              | Some subdir -> "^" ^ String.concat "/" subdir
              );
            let dir = match where.install_ocamlfind with
              | [] -> where.install_ocamllib
              | dir :: _ -> dir
            in
            really_install_META (Filename.concat dir meta_basename)
          end
      in
      iter where.install_ocamlfind

let install where what lib installdir =
  match BuildOCamlGlobals.get_by_id lib with
  | None -> ()
  | Some lib ->
    let opk = lib.lib_opk in
    Printf.eprintf "Installing %S in %S\n%!" lib.lib.lib_name installdir;
    let log = BuildUninstall.create_un () in
    let uninstall_file = Filename.concat installdir
      (Printf.sprintf "%s.uninstall" lib.lib.lib_name) in
    let save_uninstall warning =
      BuildUninstall.add_un_field log VERSION opk.opk_version;
      BuildUninstall.add_un_field log TYPE
        (BuildOCP.string_of_package_type lib.lib.lib_type);
      begin match warning with
          None -> ()
        | Some warning ->
          BuildUninstall.add_un_field log WARNING warning
      end;
      let uninstall_file_d = in_destdir where uninstall_file in
      BuildUninstall.save_un uninstall_file_d log
    in
    try
      let installbin = where.install_bindir in
      let installdir_d = in_destdir where installdir in
      if not (Sys.file_exists installdir_d) then
        safe_mkdir where log installdir;
      BuildUninstall.add_un_field log FILE uninstall_file;

      let bundle = lib.lib.lib_bundles in
      List.iter (fun pk ->
        BuildUninstall.add_un_field log PACK pk.lib_name) bundle;

    (* Do the installation *)
      let meta = MetaFile.create None in

      MetaFile.set_version meta opk.opk_version;
      MetaFile.set_description meta
        (BuildValue.get_string_with_default lib.lib_opk.opk_options "description" lib.lib.lib_name);

      let need_requires = ref false in

      let install_file file kind =
        let dst_file =
          match kind with

          | CMI when what.install_asm_lib || what.install_byte_lib ->
            Some (Filename.concat installdir file.file_basename)
          | STUB_A when what.install_asm_lib || what.install_byte_lib ->
            Some (Filename.concat installdir file.file_basename)
          | CMO when what.install_byte_lib ->
            Some (Filename.concat installdir file.file_basename)
          | CMX
          | CMX_O
          | CMXA_A when what.install_asm_lib ->
            Some (Filename.concat installdir file.file_basename)
          | CMA when what.install_byte_lib ->
            need_requires := true;
            MetaFile.add_archive meta MetaFile.precs_byte file.file_basename;
            MetaFile.add_plugin meta MetaFile.precs_byte file.file_basename ;
            MetaFile.set_exists_if meta file.file_basename;
            Some (Filename.concat installdir file.file_basename)
          | CMXA when what.install_asm_lib ->
            need_requires := true;
            MetaFile.add_archive meta MetaFile.precs_asm file.file_basename;
            Some (Filename.concat installdir file.file_basename)
          | CMXS when what.install_asm_lib ->
            need_requires := true;
            MetaFile.add_plugin meta MetaFile.precs_asm file.file_basename;
            Some (Filename.concat installdir file.file_basename)
          | RUN_ASM when  what.install_asm_bin ->
            Some (Filename.concat installbin
                    (Filename.chop_suffix file.file_basename ".asm"))
          | RUN_BYTE when what.install_byte_bin ->
            Some (Filename.concat installbin file.file_basename)

          | RUN_BYTE
          | RUN_ASM
          | CMI
          | CMO
          | CMX
          | CMX_O
          | CMXS
          | CMA
          | CMXA
          | CMXA_A
          | STUB_A
            -> None

        in
        match dst_file with
          None -> ()
        | Some dst_file ->

          let dirname = Filename.dirname dst_file in
          let dirname_d = in_destdir where dirname in
          if not (Sys.file_exists dirname_d) then
            safe_mkdir where log dirname;

        (*            Printf.fprintf stderr "\tto %S : %!" dst_file; *)
          let src_file = file_filename file in
          copy_file where log src_file dst_file
      in
      Printf.eprintf "\tfiles: %!";
      List.iter (fun (file, kind) ->
        install_file file kind
      )
        (lib.lib_byte_targets
         @ lib.lib_asm_targets
         @ lib.lib_intf_targets
         @ lib.lib_stub_targets);

      (* Requires only if it can be linked *)
      if !need_requires then
        List.iter (fun dep ->
          let olib = dep.dep_project in
          if dep.dep_link then
            MetaFile.add_requires meta [] olib.lib.lib_name
        ) lib.lib_requires;


      begin match  where.install_datadir with
        None -> ()
      | Some datadir ->
        let datadir = Filename.concat datadir lib.lib.lib_name in
        List.iter (fun file ->
          safe_mkdir where log datadir;
          let basename = Filename.basename file in
          let dst_file = Filename.concat datadir basename in
          let src_file = Filename.concat (FileGen.to_string lib.lib.lib_dirname) file in
          copy_file where log src_file dst_file
        )
          (BuildValue.get_strings_with_default lib.lib_opk.opk_options "data_files" []);

      end;


      List.iter (fun file ->
        safe_mkdir where log installdir;
        let basename = Filename.basename file in
        let dst_file = Filename.concat installdir basename in
        let src_file = Filename.concat (FileGen.to_string lib.lib.lib_dirname) file in
        copy_file where log src_file dst_file
      )
        (BuildValue.get_strings_with_default lib.lib_opk.opk_options "lib_files" []);

      List.iter (fun file ->
        safe_mkdir where log installbin;
        let basename = Filename.basename file in
        let dst_file = Filename.concat installbin basename in
        let src_file = Filename.concat (FileGen.to_string lib.lib.lib_dirname) file in
        copy_file where log src_file dst_file
      )
        (BuildValue.get_strings_with_default lib.lib_opk.opk_options "bin_files" []);
      Printf.eprintf "\n%!";

      install_META log where installdir meta lib;

      save_uninstall None;

    with
    | Unix.Unix_error(Unix.EACCES, _,_) ->
      Printf.eprintf "Error: could not install %s, permission denied\n%!"
      lib.lib.lib_name;
      exit 2

    | exn ->
      Printf.eprintf "Error: could not install %s, exception %S raised\n%!"
        lib.lib.lib_name (Printexc.to_string exn);
      (try
         save_uninstall (Some "Install partially failed");
       with exn ->
         Printf.eprintf
           "Error: Could not save uninstall log, exception %S raised\n%!"
           (Printexc.to_string exn);
      );
      exit 2

(* TODO: we might install the same package several times in different
   directories, no ? *)

let find_installdir where lib =
  (match where.install_destdir with
      None -> ()
    | Some destdir ->
      try
        FileString.safe_mkdir destdir
      with
      | _e ->
        Printf.eprintf "Error: install DESTDIR %S can be created\n%!"
          destdir;
        BuildMisc.clean_exit 2
  );

  let lib_name = lib.lib.lib_name in
  let install_subdir = BuildOCamlVariables.install_subdir.get
    lib.lib_opk.opk_options in
  let lib_name =
    if install_subdir = "" then lib_name else
      Filename.concat install_subdir lib_name
  in

    (* Check whether it is already installed : *)
  let iter possible libdirs =
    match libdirs with
      [] ->
        begin
          match possible with
            None ->
              Printf.eprintf "Error: no directory where to install files\n%!";
              None
          | Some installdir ->
            Some installdir
        end

    | libdir :: _libdirs ->
      let installdir = Filename.concat libdir lib_name in
      let installdir_d = in_destdir where installdir in

      (* TODO: we should just check that we have write permission to that
         directory *)
      Some installdir_d

        (*
      if Sys.file_exists installdir_d then (* Found ! *)
        begin
(*
          TODO: we should copy all files from this directory to a
          sub-directory "_attic". Can we disable .ocp files within them ?
*)
            (* TODO: shouldn't we check for an .uninstall file ? *)
          Printf.eprintf "Error: package %S seems already installed in\n"
            lib_name;
          Printf.eprintf "\t%S\n%!" installdir;
          None
        end
      else
        begin
          match possible with
          | None ->
            let testlog = ref [] in
            if
              (try
                 safe_mkdir where testlog installdir;
                 true
               with _ -> false)
            then begin
              List.iter (function
              | (DIR, dir) ->
                let dir_d = in_destdir where dir in
                MinUnix.rmdir dir_d
              | _ -> assert false) !testlog;
              iter (Some installdir) libdirs
            end else begin
              Printf.eprintf
                "Warning: skipping install dir %S, not writable.\n%!"
                libdir;
              iter None libdirs
            end
          | Some _ ->
            iter possible libdirs
        end
*)
  in
  iter None where.install_libdirs



open BuildOptions
open BuildOCamlConfig.TYPES

let install_where cin cout =

  let install_bindir = match cin.cin_install_bin, cout.cout_ocamlbin with
        None, Some dir ->   dir
      | Some dir, _ -> dir
      | None, None ->
        Printf.eprintf "Error: you must specify the bindir to install/uninstall files\n%!";
        BuildMisc.clean_exit 2
  in
  let install_ocamllib = match cout.cout_ocamllib with
    None ->
      Printf.eprintf "Error: you must specify the ocaml libdir to install/uninstall files\n%!";
      BuildMisc.clean_exit 2
    | Some dir -> dir
  in

  if Filename.is_relative install_bindir then begin
    Printf.eprintf "Error: install-bin %S is not absolute\n%!" install_bindir;
    exit 2
  end;
  let install_libdirs =  match cin.cin_install_lib with
        None ->
        begin match cout.cout_meta_dirnames with
            [] -> begin
              match cout.cout_ocamllib with
                None -> []
              | Some ocamllib -> [ocamllib]
            end
          | _ -> cout.cout_meta_dirnames
        end
    | Some dir -> [dir] in
  List.iter (fun install_lib ->
    if Filename.is_relative install_lib then begin
      Printf.eprintf "Error: install-bin %S is not absolute\n%!" install_lib;
      exit 2
    end;
  ) install_libdirs;

  let install_ocamlfind = match cin.cin_install_meta with
    | None -> cout.cout_meta_dirnames
    | Some dir -> [dir]
  in
  List.iter (fun install_lib ->
    if Filename.is_relative install_lib then begin
      Printf.eprintf "Error: install-meta %S is not absolute\n%!" install_lib;
      exit 2
    end;
  ) install_ocamlfind;

  {
    install_destdir = cin.cin_install_destdir;
    install_libdirs;
    install_bindir;
    install_datadir = cin.cin_install_data;

    install_ocamllib;
    install_ocamlfind;
  }

let install_what () =

    {
      install_asm_bin = true;
      install_byte_bin = true;
      install_asm_lib = true;
      install_byte_lib = true;
    }
