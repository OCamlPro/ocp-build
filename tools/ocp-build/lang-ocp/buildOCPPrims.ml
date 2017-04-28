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
open BuildValue.TYPES
open BuildOCPTree

module Init(S: sig

    val filesubst : (string * env list) BuildSubst.t

  end) = struct

let primitives = ref StringMap.empty
let add_primitive s help ( f : env list -> env -> plist) =
  let f envs env =
    try
      f (env :: envs) env
    with e ->
      Printf.eprintf "Warning: exception raised while running primitive %S\n%!" s;
      raise e
  in
  primitives := StringMap.add s (f, help) !primitives

let add_function s help f =
  let f _ env =
    try
      f [ env ] env
    with e ->
      Printf.eprintf "Warning: exception raised while running primitive %S\n%!" s;
      raise e
  in
  primitives := StringMap.add s (f, help) !primitives

let eprint_env indent env =
  let b = Buffer.create 1000 in
  BuildValue.bprint_env b indent env;
  Printf.eprintf "%s%!" (Buffer.contents b)

let filesubst = S.filesubst

let _ =
  let subst_files envs to_file =

    let files = BuildValue.prop_list (BuildValue.get_local envs "files") in
    let from_ext = BuildValue.get_strings_with_default envs "from_ext" [] in
    let keep = BuildValue.get_bool_with_default envs "keep_others" false in
    let files = List.fold_left (fun files (file, env) ->
      try
        let pos = String.index file '.' in
        if from_ext = [] || (
          let ext = String.sub file pos (String.length file - pos) in
          List.mem ext from_ext) then
          let file = BuildSubst.apply_substituter filesubst
            to_file (file,envs)
          in
          (* Printf.eprintf "subst to %S\n%!" file; *)
          (file, env) :: files
        else raise Not_found
      with Not_found ->
        if keep then
          (file,env) :: files
        else files
    ) [] files in
    BuildValue.value (List.rev files)
  in

  let subst_file envs ( _env : env) =
    let to_ext = BuildValue.get_strings_with_default envs "to_ext" [] in
    let to_file = match to_ext with
        [ to_ext ] -> "%{dirname}%/%{basename}%" ^ to_ext
      | _ ->
        try
          BuildValue.string_of_plist (BuildValue.get_local envs "to_file")
        with Var_not_found _ ->
          failwith "%subst_ext: to_ext must specify only one extension"
    in

    subst_files envs to_file
  in
  let subst_help =     [
    "Perform a substitution on a list of files";
    "ENV can contain:";
    "- files: the list of files";
    "- to_file: the destination, with substitutions";
    "- to_ext: an extension, if only the extension should be changed";
    "- from_ext: perform only on files ending with these extensions";
    "- keep_others: true if non-substituted files should be kept";
  ]  in
  add_primitive "subst_ext" subst_help subst_file;
  add_primitive "subst_file" subst_help subst_file;

  add_primitive "basefiles" [] (fun envs _env ->
    subst_files envs "%{basefile}%"
  );

  add_primitive "path" []
    (fun envs env ->
      let path = BuildValue.get_strings envs "path" in
      let s =
        match path with
          [] -> ""
        | dirname :: other_files ->
          List.fold_left (fun path file ->
            Filename.concat path file
          ) dirname other_files
      in
      BuildValue.value [ s, env ]
    );

  add_primitive "string" [
    "Returns the concatenation of a list of strings";
    "ENV must contain:";
    "- strings : the list of strings";
    "ENV can contain:";
    "- sep : a separator, to be added between strings";
  ]
    (fun envs env ->
      let path = BuildValue.get_strings envs "strings" in
      let sep = BuildValue.get_string_with_default envs "sep" "" in
      BuildValue.value [ String.concat sep path, env ]
    );

  add_primitive "mem" [
    "Check if a string is included in a list of strings";
    "ENV must contain:";
    "- string : the string";
    "- strings : the list of strings";
  ]
    (fun envs _env ->
      let string = BuildValue.get_string envs "string" in
      let strings = BuildValue.get_strings envs "strings" in
      let bool = List.mem string strings in
      BuildValue.plist_of_bool bool
    );


  add_function "disp" [
    "Display its environment ENV"
  ]
    (fun _envs env ->
      Printf.printf "disp:\n%!";
      eprint_env "" env;
      BuildValue.value []
    );

  add_function "exit" []
    (fun envs _env ->
      let code = BuildValue.get_local_string_with_default envs "code" "0" in
      exit (int_of_string code)
    );

  add_function "pack" [] (fun envs _env ->
    let to_module = BuildValue.get_local envs "to_module" in
    let files = BuildValue.get_local_prop_list envs "files" in

    let (packmodname, pack_env) =
      match to_module with
      | VList [ VTuple [VString (packmodname,_);
                        VObject pack_env] ]
      | VTuple [VString (packmodname,_);
                VObject pack_env]  ->
        packmodname, pack_env
      | VString (packmodname,_) -> packmodname, BuildValue.empty_env
      | _ -> failwith
        "%pack with wrong argument types, should be %pack(to_module = modname, files = [...])"
    in
    let modnames = ref [] in

    let files = List.map (fun (file, file_env) ->
      file,
      BuildValue.set_strings file_env "packed"
        (packmodname ::
           (try
              BuildValue.get_strings [ file_env ] "packed"
            with Var_not_found _ ->
              modnames := Filename.basename file :: !modnames;
              []))
    ) files in

    let pack_env = BuildValue.set_strings pack_env "pack" (List.rev !modnames) in

    BuildValue.value (files @
      [ packmodname ^ ".ml", pack_env ])
  );

  add_function "dstdir" [
    "Replaced by %{package_FULL_DST_DIR}%";
    "ENV must contain:";
    "- p : the package";
    "ENV can contain:";
    "- file : a filename that will be appended";
  ] (fun envs _env ->
    let p = BuildValue.get_local_string envs "p" in
    let s = Printf.sprintf "%%{%s_FULL_DST_DIR}%%" p in
    let s = try
              let file = BuildValue.get_local_string envs "file" in
              Filename.concat s file
      with Var_not_found _ -> s
    in
    VString (s, StringRaw)
  );

  add_function "srcdir" [
    "Replaced by %{package_FULL_SRC_DIR}%";
    "ENV must contain:";
    "- p : the package";
    "ENV can contain:";
    "- file : a filename that will be appended";
  ] (fun envs _env ->
    let p = BuildValue.get_local_string envs "p" in
    let s = Printf.sprintf "%%{%s_FULL_SRC_DIR}%%" p in
    let s =try
             let file = BuildValue.get_local_string envs "file" in
             Filename.concat s file
      with Var_not_found _ -> s
    in
    VString (s, StringRaw)
  );

  add_function "byte_exe" [] (fun envs _env ->
    let p = BuildValue.get_local_string envs "p" in
    let s = Printf.sprintf "%%{%s_FULL_DST_DIR}%%/%s.byte" p p in
    VString (s, StringRaw)
  );

  add_function "asm_exe" [] (fun envs _env ->
    let p = BuildValue.get_local_string envs "p" in
    let s = Printf.sprintf "%%{%s_FULL_DST_DIR}%%/%s.asm" p p in
    VString (s, StringRaw)
  );

  add_function "split" [
    "Cut a string into a list of strings, at a given char,";
    "  empty strings are kept.";
    "ENV must contain:";
    "- s : the string to be cut";
    "ENV can contain:";
    "- sep : a string, whose first char will be the separator";
    "    (default to space)";
  ]
    (fun envs _env ->
      let s = BuildValue.get_string envs "s" in
      let sep = BuildValue.get_string_with_default envs "sep" " " in
      let sep = if sep = "" then ' ' else sep.[0] in
      VList (List.map (fun s ->
        VString (s, StringRaw)) (OcpString.split s sep))
    );

  add_function "split_simplify" [
    "Cut a string into a list of strings, at a given char,";
    "  empty strings are removed.";
    "ENV must contain:";
    "- s : the string to be cut";
    "ENV can contain:";
    "- sep : a string, whose first char will be the separator";
    "    (default to space)";
  ] (fun envs _env ->
    let s = BuildValue.get_string envs "s" in
    let sep = BuildValue.get_string_with_default envs "sep" " " in
    let sep = if sep = "" then ' ' else sep.[0] in
    VList (List.map (fun s ->
      VString (s, StringRaw)) (OcpString.split_simplify s sep))
  );

  let uniq_counter = ref 0 in
  add_function "uniq" [
    "Returns a uniq string, to be used as a uniq identifier";
  ] (fun _ _ ->
    incr uniq_counter;
    VString (Printf.sprintf ".id_%d" !uniq_counter,
             StringRaw));
  ()

let primitives_help () =
  StringMap.map (fun (_,h) -> h) !primitives



end
