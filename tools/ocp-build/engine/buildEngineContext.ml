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
open BuildEngineTypes
open BuildEngineGlobals

(* all this mechanism is used to get rid of symbolic links and
   wrongly formatted paths. Instead of normalizing paths, we try
   to discover equivalent directories by checking their file-uids.

   This will fail under Windows with remote file-systems. In this case,
   we should fallback to normalization, isn't it ?
*)

(* TODO do this somewhere
   - On Windows, st_ino always returns 0. So, we need another way of finding
   equivalent directories.
   - On Windows, we have no way to identify identical directories. Let's try this:
   first, create a special file in our top directory. now, in every directory,
   check whether we have reached that directory. If yes, we know where we are !

   On Windows, for remote directories, such as the ones created by mounting
   Linux directories within a Windows Virtualbox, nFileIndex does not work
   at all. In this case, just use the directory name ! We should check which
   filesystem we are dealing with, and decide to use or not fileuids... In
   the meantime, we just use filenames when possible.

*)

let on_Windows = Sys.os_type = "Win32"

let use_file_uid = try
                     ignore (Sys.getenv "OCPBUILD_BY_FILENAME"); false
  with Not_found -> true

let fileuid_by_name = Hashtbl.create 1111
let fileuid = ref 1

let rec normalize path root =
  match path with
  | [] -> List.rev root
  | ".." :: path -> normalize path (List.tl root)
  | "." :: path -> normalize path root
  | "" :: path -> normalize path root
  | dir :: path -> normalize path (dir :: root)

(* Since this would only be used on Windows, replace / by \\,
   and prevent double \\, and ending \\ *)
let normalize filename =
  let filename =
    if Filename.is_relative filename then
      let curdir = Sys.getcwd () in
      Filename.concat curdir filename
    else
      filename
  in
  let len = String.length filename in
  let filename = Bytes.of_string filename in
  for i = 0 to len-1 do
    if Bytes.get filename i = '\\' then filename.[i] <- '/'
  done;
  let filename = Bytes.to_string filename in
  let path = OcpString.split filename '/' in
  String.concat "/" (
    match path with
    | partition :: path when
        String.length partition = 2 &&
                          partition.[1] = ':' -> partition :: normalize path []
    | path -> normalize path []
  )

let normalize filename =
  let newname = normalize filename in
  if verbose 5 then
    Printf.eprintf "normalize(%S) = %S\n%!" filename newname;
  newname

let get_file_uid filename =
  let st = MinUnix.stat filename in
  (st,
   if st.MinUnix.st_ino = 0 &&
     match MinUnix.os_type with
       MinUnix.UNIX -> false
     | MinUnix.CYGWIN | MinUnix.WINDOWS -> true
       then
         if use_file_uid then
           let ft = OnlyWin32.getFileInformationByName filename in
           (ft.OnlyWin32.dwVolumeSerialNumber, ft.OnlyWin32.nFileIndex)
         else
           let filename = normalize filename in
           try
             Hashtbl.find fileuid_by_name filename
           with Not_found ->
             incr fileuid;
             let fileuid = Int64.of_int !fileuid in
             Hashtbl.add fileuid_by_name filename (0,fileuid);
             (0, fileuid)
       else
         (st.MinUnix.st_dev, Int64.of_int st.MinUnix.st_ino)
   )



let find_directory b dirname =
  if verbose 5 then
    Printf.fprintf stderr
      "BuildEngineContext.find_directory %s\n"
      dirname;
  let _, key = get_file_uid dirname in
  Hashtbl.find b.build_directories key

  (* BUG on Windows: if ocp-build is run in a directory that is
     a Cygwin symlink to another directory (in another partition),
     it cannot recover and end with an assert failure, probably
     because it tries to Sys.readdir a non-existing directory,
     which does not fail on Windows ! *)

let rec add_directory b filename =
  if verbose 5 then
    Printf.fprintf stderr
      "BuildEngineContext.add_directory %s\n"
      filename;
  let st, key = get_file_uid filename in
  if verbose 5 then
    Printf.fprintf stderr
      "BuildEngineContext.add_directory |%s| (%d,%Lx)\n"
      filename (fst key) (snd key);
  try
    let dir = Hashtbl.find b.build_directories key in
    if verbose 5 then
      Printf.fprintf stderr "Found\n";
    dir
  with Not_found ->
    if verbose 5 then
      Printf.fprintf stderr "Not found\n";
    let dir =
      let dirname = Filename.dirname filename in
      if verbose 5 then
        Printf.fprintf stderr "\tdirname = %s\n" filename;
      match st.MinUnix.st_kind with
      | MinUnix.S_LNK ->
          let link = OnlyUnix.readlink filename in
          let filename =
            if Filename.is_relative link then
              Filename.concat dirname link
            else link
          in
          add_directory b filename
      | MinUnix.S_DIR -> begin
        let basename = Filename.basename filename in
        if verbose 5 then
          Printf.fprintf stderr "\tfilename = %s\n" filename;
        if dirname = filename then
          let rec dir = {
            dir_key = Inode key;
            dir_id = new_dir_id b;
            dir_basename = basename;
            dir_parent = dir;
            dir_file = FileGen.of_string (if on_Windows then dirname else "/");
            dir_files = StringMap.empty;
            dir_dirs = StringMap.empty;
            dir_fullname = filename;
          } in
          dir
        else
          let parent_dir = add_directory b dirname in
          match basename with
            "." -> parent_dir
          | ".." -> parent_dir.dir_parent
          | _ ->
              (* educated guess : this does not really make sense on Windows,
                 as inode numbers are generated and not read from the file-system. *)
            let dirname = parent_dir.dir_fullname in
            let basename =
              try
                let _, key2 = get_file_uid (Filename.concat dirname basename) in
                if key = key2
                then
                  basename
                else raise Not_found
              with _ ->
                if verbose 5 then
                  Printf.eprintf "Sys.readdir(%S)\n%!" dirname;
                let files = Sys.readdir dirname in
                let nfiles = Array.length files in
                let rec iter i =
                  if i = nfiles then begin
                    Printf.eprintf "Error: ocp-build failed to discover the uniq identifier\n";
                    Printf.eprintf "  of some of the files it is working with. This can happen\n";
                    Printf.eprintf "  when using file-systems without uniq file identifiers.\n";
                    Printf.eprintf "  You should set OCPBUILD_BY_FILENAME=1 to use filename\n";
                    Printf.eprintf "  normalization.\n%!";
                    exit 2;
                  end;
                  let file = files.(i) in
                  let _, key2 =           get_file_uid  (Filename.concat dirname file) in
                  if verbose 5 then
                    Printf.eprintf "Compare key with %S (%d,%Lx)\n%!" file (fst key2) (snd key2);
                  if key = key2 then
                    file
                  else
                    iter (i+1)
                in
                iter 0
            in
            let dir =
              try
                StringMap.find basename parent_dir.dir_dirs
              with Not_found ->
                let dir = {
                  dir_basename = basename;
                  dir_parent = parent_dir;
                  dir_file = FileGen.add_basename parent_dir.dir_file basename;
                  dir_key = Inode key;
                  dir_id = new_dir_id b;
                  dir_files = StringMap.empty;
                  dir_dirs = StringMap.empty;
                  dir_fullname = Filename.concat parent_dir.dir_fullname basename;
                } in
                parent_dir.dir_dirs <- StringMap.add basename dir parent_dir.dir_dirs;
                dir
            in
            dir
      end
      | _ -> assert false
    in
    Hashtbl.add b.build_directories key dir;
    dir

let find_dir dir basename =
  StringMap.find basename dir.dir_dirs

let find_file dir basename =
  StringMap.find basename dir.dir_files










  (* Convert all filenames starting with the drive name to Unix filenames,
     on Cygwin *)
let truename dirname =
  match MinUnix.os_type with
    MinUnix.CYGWIN ->
      let len = String.length dirname in
      if (len > 1 && dirname.[1] = ':' &&
          match dirname.[0] with
            'a'..'z' | 'A'..'Z' -> true
          | _ -> false)
      then
        Printf.sprintf "/cygdrive/%c%s"
          (Char.lowercase dirname.[0])
          (String.sub dirname 2 (len-2))
      else
        dirname
  | _ -> dirname

let add_directory b dirname = add_directory b (truename dirname)
let find_directory b dirname = find_directory b (truename dirname)




  (* let build_dir = add_directory b build_dir_filename *)

let add_any_file file_package dir filename file_kind =
  let b = file_package.package_context in
  let dirname = Filename.dirname filename in
  let basename = Filename.basename filename in
  let dir = if dirname = "." || dirname = "" then dir else
      add_directory b (Filename.concat dir.dir_fullname dirname)
  in
  try find_file dir basename with Not_found ->
    let file = {
      file_id = new_file_id b;
      file_kind = file_kind;
      file_basename = basename;
      file_dir = dir;
      file_file = FileGen.add_basename dir.dir_file basename;
      file_exists = false; (* shall we do that now ? *)
      file_mtime = BuildMtime.zero;
      file_target_of = [];
      file_source_for = [];
      file_package;
    } in
    file_package.package_files <- IntMap.add file.file_id file
      file_package.package_files;
    dir.dir_files <- StringMap.add basename file dir.dir_files;
    Hashtbl.add b.build_files file.file_id file;
    file

let add_virtual_file b dir basename =
  add_any_file b dir basename FILE_VIRTUAL

let make_virtual_file f =
  f.file_kind <- FILE_VIRTUAL

let add_temp_file b dir basename =
  add_any_file b dir basename FILE_TEMPORARY

let add_file b dir basename =
  add_any_file b dir basename FILE_REAL

let create current_dir_filename build_dir_filename =
    (*  Printf.eprintf "BuildEngineContext.create %s\n%!" build_dir_filename; *)
  let (build_rules : (int, build_rule) Hashtbl.t) = Hashtbl.create 1111 in
  let (build_files : (int, build_file) Hashtbl.t) = Hashtbl.create 1111 in
  let (build_directories : (int * int64,   build_directory) Hashtbl.t) = Hashtbl.create 1111 in

  let build_cache_content = ref 0 in
  let build_cache = ref DigestMap.empty in
  let build_cache_filename = Filename.concat build_dir_filename "cache.cmd" in
    (*  Printf.fprintf stderr "build_cache_filename = %s\n%!" build_cache_filename; *)
  begin
    match try
            let ic = open_in build_cache_filename in
            Some ic
      with _e -> None
    with
      None -> ()
    | Some ic ->
      try
        while true do
          let line = input_line ic in
          if String.length line > 0 then
            match line.[0] with
              '#' -> ()
            | _ ->
              let targets, command = OcpString.cut_at line ' ' in
              let targets = OcpDigest.of_hex targets in
              let command = OcpDigest.of_hex command in
              incr build_cache_content;
              build_cache := DigestMap.add targets command !build_cache;
        done
      with End_of_file -> close_in ic
  end;
  let build_cache_log = open_out (build_cache_filename ^ ".log") in
  let build_log = open_out
    (Filename.concat build_dir_filename "build.log") in
  if verbose 5 then
    Printf.eprintf "Cache: %d digests loaded\n" !build_cache_content;
  let b =
    {
      build_should_restart = false;
      build_directories;
      build_files;
      build_rules;
      build_packages = IntMap.empty;

      build_next_dir_id = 0;
      build_next_file_id = 0;
      build_next_rule_id = 0;
      build_next_process_id = 0;
      build_next_package_id = 0;

      build_dir_filename = build_dir_filename;                   (* "/..../_obuild" *)
      build_dir_basename = Filename.basename build_dir_filename; (* "_obuild" *)
      build_dir = FileGen.of_string build_dir_filename;

      build_log = build_log;

      build_cache_input = !build_cache;
      build_cache_entries = IntMap.empty;
      build_cache_filename = build_cache_filename;
      build_cache_log = build_cache_log;

      cross_arg = None;
      stop_on_error_arg = true;

        (* to display progress *)
      build_stats_to_execute = 0;
      build_stats_executed = 0;
      build_stats_running_rules = IntMap.empty;
      build_stats_lastpoint = 0;

      queue_inactive = [];
      queue_ready = IntMap.empty;
      queue_waiting = IntMap.empty;
      queue_not_waiting = IntMap.empty;
      temp_files = IntMap.empty;
      unmanaged_dependencies = [];
      fatal_errors = [];
      errors = [];
      stats_command_executed = 0;
      stats_files_generated = 0;
      stats_total_time = 0.;
      build_create_dirs = [];
    }

  in
    (*  Printf.eprintf "add_directory MASTER: %s\n" current_dir_filename; *)
  let dir = add_directory b current_dir_filename in
  dir.dir_fullname <- ".";
  dir.dir_file <- FileGen.of_string ".";
  let dir2 = add_directory b current_dir_filename in
  assert (dir == dir2);
  b
;;

let new_package b package_name =
  let package_uid = b.build_next_package_id in
  b.build_next_package_id <- b.build_next_package_id + 1;
  let p = {
    package_uid;
    package_package = package_name;
    package_context = b;
    package_files = IntMap.empty;
  } in
  b.build_packages <- IntMap.add package_uid p b.build_packages;
  p
