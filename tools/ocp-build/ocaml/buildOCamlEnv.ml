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

(* STATUS:
   - there is an ambiguity between "vmthreads-stdlib" and "stdlib", because
     they exports the same interface. However, "vmthreads-stdlib" does not
     exist in native code.
   - there is a cycle of dependencies between "vmthreads-unix" and
     "vmthreads-stdlib", because Pervasives uses types from Unix.
   - we should extract common subsets, give them a name (maybe it can
     be infered automatically), and use that name when possible.
*)

(* TODO:
  - implement use of "provide" in ocp-build
  - implement supersets : a library can provide a superset of another
     library.
  - define meta-libraries, taking sub-parts of other libraries, so that
     they can be defined as common subsets
*)

open OcpCompat

open BuildObjectInspector

type library = {
  mutable lib_name : string;
  lib_basename : string;
  mutable lib_provide : string list;
  lib_dirname : string;

  mutable lib_asm_exists : bool;
  mutable lib_byte_exists : bool;
  mutable lib_byte_mods : (string * Digest.t) list;
  mutable lib_asm_mods : (string * Digest.t) list;
  mutable lib_deps : (string * Digest.t * bool) list;

  mutable lib_includes : library list;
  mutable lib_included_in : library list;
}

let dirs = ref []


let null_digest = (String.make 16 '\000' : Digest.t)

let filenames = Hashtbl.create 113
let mods = Hashtbl.create 113
let libnames = ref StringMap.empty

let add_mod modname digest native lib =
  try
    let libs = Hashtbl.find mods (modname, digest, native) in
    match !libs with
      lib1 :: _ when lib1 == lib -> ()
    | _ -> libs := lib :: !libs
  with Not_found ->
    Hashtbl.add mods (modname, digest, native) (ref [lib])

let add_asm_library lib filename =
  match BuildObjectInspector.load_object_file filename with
  | CMXA desc ->
    lib.lib_asm_exists <- true;
    let cmx_deps = Hashtbl.create 113 in
    let add_cmx_dep (modname, digest) =
      if not (Hashtbl.mem cmx_deps (modname, digest)) then begin
        Hashtbl.add cmx_deps (modname, digest) ();
        lib.lib_deps <- (modname, digest, true) :: lib.lib_deps
      end
    in

    (* declare modules *)
    List.iter (fun (cmx, digest) ->
      lib.lib_asm_mods <- (cmx.ui_name, digest) :: lib.lib_asm_mods;
      Hashtbl.add cmx_deps (cmx.ui_name, digest) ();
      add_mod cmx.ui_name digest true lib;
      add_mod cmx.ui_name null_digest true lib;
    ) desc.cmxa_units;

    (* find external dependencies *)
    List.iter (fun (cmx, digest) ->
      List.iter add_cmx_dep cmx.ui_imports_cmx;
    ) desc.cmxa_units
  | _ -> assert false

let add_byte_library lib filename =
  match BuildObjectInspector.load_object_file filename with
  | CMA desc ->
    lib.lib_byte_exists <- true;

    let cmx_deps = Hashtbl.create 113 in

      (* declare modules *)
    List.iter (fun cmo ->
      let digest = List.assoc cmo.cu_name cmo.cu_imports in
      Hashtbl.add cmx_deps (cmo.cu_name, digest) ();
      lib.lib_byte_mods <- (cmo.cu_name, digest) :: lib.lib_byte_mods;
      add_mod cmo.cu_name digest false lib;
    ) desc.cma_units;


    if not lib.lib_asm_exists then
      let add_cmx_dep (modname, digest) =
        if not (Hashtbl.mem cmx_deps (modname, digest)) then begin
          Hashtbl.add cmx_deps (modname, digest) ();
          lib.lib_deps <- (modname, digest, false) :: lib.lib_deps
        end
      in

      (* find external dependencies *)
      List.iter (fun cmo ->
        List.iter add_cmx_dep cmo.cu_imports;
      ) desc.cma_units


  | _ -> assert false

let add_new_library filename =
  if Filename.check_suffix filename ".p" then () else
  let lib_name = Filename.basename filename in
  let lib_dirname = Filename.dirname filename in

  let lib = {
    lib_name = lib_name;
    lib_basename = lib_name;
    lib_dirname = lib_dirname;
    lib_provide = [];

    lib_asm_exists = false;
    lib_byte_exists = false;
    lib_asm_mods = [];
    lib_byte_mods = [];
    lib_deps = [];

    lib_includes = []; (* provides *)
    lib_included_in = []; (* provided by *)
  } in

  Hashtbl.add filenames filename lib;

  begin try
          let libs = StringMap.find lib_name !libnames in
          libs := lib :: !libs
    with Not_found ->
      libnames := StringMap.add lib_name (ref [lib])  !libnames
  end;

  let asm_filename = filename ^ ".cmxa" in
  if Sys.file_exists asm_filename then
    add_asm_library lib asm_filename;

  let byte_filename = filename ^ ".cma" in
  if Sys.file_exists byte_filename then
    add_byte_library lib byte_filename;

  ()


let add_library filename =
  if not (Hashtbl.mem filenames filename) then
    add_new_library filename


let add_bytecode_library filename =
  Printf.printf "add_bytecode_library %s\n%!" filename;
  add_library (Filename.chop_suffix filename ".cma")

let add_native_library filename =
  Printf.printf "add_native_library %s\n%!" filename;
  add_library (Filename.chop_suffix filename ".cmxa")


let map =
  let map = ref StringMap.empty in
  map := StringMap.add "cma" add_bytecode_library !map;
  map := StringMap.add "cmxa" add_native_library !map;
  !map

let scan_directory dirname =
  Printf.printf "scan_directory %s\n%!" dirname;
  BuildScanner.scan_directory_for_extensions dirname map











let find_longname lib =
  let rec iter dirname lib_name =
    if List.mem dirname !dirs then lib_name else
      let basenane = Filename.basename dirname in
      if basenane = "" then lib_name else
        let dirname = Filename.dirname dirname in
        iter dirname (Printf.sprintf "%s-%s" basenane lib_name)
  in
  iter lib.lib_dirname lib.lib_name

let generate_ocp filename =
  let oc = open_out filename in
  Printf.fprintf oc "(* generated by ocp-build-infer-env *)\n";
  Printf.fprintf oc "begin\n";
  Printf.fprintf oc "  generated = true\n";

  (* Generate uniq names for libraries *)
  StringMap.iter (fun lib_name libs ->
    match !libs with
      [ lib ] -> ()
    | libs ->

      let new_names = ref StringMap.empty in
      List.iter (fun lib ->
        let long_name = find_longname lib in
        try
          let libs = StringMap.find long_name !new_names in
          libs := lib :: !libs
        with Not_found ->
          new_names := StringMap.add long_name (ref [lib]) !new_names
      ) libs;

      let counter = ref 0 in
      let rename_lib lib =
        incr counter;
        lib.lib_name <- Printf.sprintf "%s%d" lib_name !counter
      in
      StringMap.iter (fun long_name libs ->

        if StringMap.mem long_name !libnames && long_name <> lib_name then
          List.iter rename_lib !libs
        else
          match !libs with
            [ lib ] ->
              lib.lib_name <- long_name
          | libs ->
            List.iter rename_lib libs
      ) !new_names

  ) !libnames;

  (* Check inclusions *)
  let check_included_in lib modules native =
    match modules with
      [] -> () (* weird *)
    | (modname, digest) :: modules ->
      try
        let libs = Hashtbl.find mods (modname, digest, native) in
        let included_in = ref [] in
        try
          List.iter (fun l ->
            if l != lib then included_in := l :: !included_in) !libs;
          List.iter (fun (modname, digest) ->
            if !included_in = [] then raise Exit;
            let libs = Hashtbl.find mods (modname, digest, native) in
            let libs = !libs in
            let prev = !included_in in
            included_in := [];
            List.iter (fun l ->
              if List.memq l libs then
                included_in := l :: !included_in
            ) prev
          ) modules;
          lib.lib_included_in <- !included_in;
          List.iter (fun l ->
            l.lib_includes <- lib :: l.lib_includes
          ) !included_in
        with Exit -> ()
      with Not_found -> assert false
  in

  StringMap.iter (fun _ libs ->
    List.iter (fun lib ->
      if lib.lib_asm_exists then
        check_included_in lib lib.lib_asm_mods true
      else
        check_included_in lib lib.lib_byte_mods false
    ) !libs
  ) !libnames;



  let declare_library lib =

    Printf.fprintf oc "  begin library %S\n" lib.lib_name;
(*
    begin match lib.lib_provide with
    | Some lib_name when lib_name <> lib.lib_name ->
      Printf.fprintf oc "    provide = %S\n" lib_name;
    | None | Some _ -> ()
    end;
*)
    if lib.lib_basename <> lib.lib_name then
      Printf.fprintf oc "    basename = %S\n" lib.lib_basename;
    Printf.fprintf oc "    dirname = %S\n" lib.lib_dirname;

    if not lib.lib_asm_exists then
      Printf.fprintf oc "    has_asm = false\n";
    if not lib.lib_byte_exists then
      Printf.fprintf oc "    has_byte = false\n";

    let requires = ref [] in
    let asm_deps = Hashtbl.create 113 in
    Hashtbl.add asm_deps lib.lib_name ();

    let add_require lib_name =
      if not (Hashtbl.mem asm_deps lib_name) then begin
        Hashtbl.add asm_deps lib_name ();
        requires := lib_name :: !requires
      end
    in

    let forced_libs = ref [] in
    let not_found = ref [] in
    let ambiguous = ref [] in
    List.iter (fun (modname, digest, native) ->
      try
        match ! (Hashtbl.find mods (modname, digest, native)) with
          [ lib ] ->
            forced_libs := lib :: !forced_libs;
            add_require lib.lib_name
        | libs ->

          ambiguous := (modname, digest, libs) :: !ambiguous
      with Not_found ->
        not_found := (modname, digest) :: !not_found
    ) lib.lib_deps;

    let added = ref true in
    while !added do
      added := false;
      let rem = !ambiguous in
      ambiguous := [];
      List.iter (fun (modname, digest, libs) ->
        let libs = List.filter (fun lib -> not (List.memq lib !forced_libs)) libs in
        match libs with
          [ lib ] ->
            forced_libs := lib :: !forced_libs; added := true;
            add_require lib.lib_name
        | [] -> ()
        | _ -> ambiguous := (modname, digest, libs) :: !ambiguous
      ) rem;
    done;

(*
    let rem = !ambiguous in
    ambiguous := [];
    List.iter (fun (modname, digest, libs) ->
      match libs with
      | { lib_name = lib_name ;
          lib_provide = Some lib_provide } :: libs ->
        if List.for_all (fun lib -> lib.lib_provide = lib.lib_provide )
          libs
        then
          add_require lib_provide
        else
          ambiguous := (modname, digest, libs) :: !ambiguous
      | _ -> ambiguous := (modname, digest, libs) :: !ambiguous
    ) rem;
*)

    List.iter (fun (modname, digest) ->
      Printf.fprintf oc "      (* provided: %s %s *) \n"
        modname (OcpDigest.to_hex digest)
    ) (if lib.lib_asm_exists then lib.lib_asm_mods else lib.lib_byte_mods);

    List.iter (fun (modname, digest, native) ->
      Printf.fprintf oc "      (* uses: %s %s *) \n"
        modname (OcpDigest.to_hex digest)
    ) lib.lib_deps;

    List.iter (fun (modname, digest, libs) ->
      Printf.fprintf oc "      (* ambiguous: %s %s [%s] *) \n"
        modname (OcpDigest.to_hex digest)
        (String.concat "," (List.map (fun lib -> lib.lib_name) libs))
    ) !ambiguous;

    List.iter (fun (modname, digest) ->
      Printf.fprintf oc "      (* not found: %s %s *) \n"
        modname (OcpDigest.to_hex digest)
    ) !not_found;

    List.iter (fun lib ->
      Printf.fprintf oc "      (* provides : %S *)\n" lib.lib_name;
    ) lib.lib_includes;

    List.iter (fun lib ->
      Printf.fprintf oc "      (* included in : %S *)\n" lib.lib_name;
    ) lib.lib_included_in;

    if !requires <> [] then begin
      Printf.fprintf oc "    requires = [ ";
      List.iter (fun lib_name -> Printf.fprintf oc "%S " lib_name) !requires;
      Printf.fprintf oc "]\n";
    end;


    Printf.fprintf oc "  end\n";
  in

  StringMap.iter (fun lib_name libs ->
    List.iter declare_library !libs
  ) !libnames;

  Printf.fprintf oc "end\n";
  close_out oc








let filename = ref "installed.ocp"

let arg_list = []
let arg_anon s = dirs := s :: !dirs
let arg_usage = " : generate installed.ocp from installation directories"

let _ =
  Arg.parse arg_list arg_anon arg_usage;

  let dirs = List.rev !dirs in
  List.iter scan_directory dirs;
  generate_ocp !filename
