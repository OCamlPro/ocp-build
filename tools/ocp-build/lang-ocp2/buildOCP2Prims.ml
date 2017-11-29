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
open BuildOCP2Tree

let features = ref BuildValue.empty_env
let queried_features = ref StringMap.empty

let with_feature_value name value =
  let env =
    try
      let f = BuildValue.get [!features] name in
      match f with
      | VObject env -> env
      | _ -> assert false
    with Var_not_found _ -> BuildValue.empty_env
  in
  let env = BuildValue.set env "enabled" (VBool true) in
  let env = BuildValue.set env "value" (VString (value,StringRaw)) in
  let env = BuildValue.set_bool env "user" false in
  features := BuildValue.set !features name (VObject env)

let with_feature name =
  let name, value = OcpString.cut_at name '=' in
  with_feature_value name value

let without_feature name =
  let env = BuildValue.empty_env in
  let env = BuildValue.set env "enabled" (VBool false) in
  let env = BuildValue.set_bool env "user" false in
  features := BuildValue.set !features name (VObject env)

let vstring s = VString (s, StringRaw)

let fatal_error loc =
  Printf.kprintf (fun s ->
      Printf.eprintf "File %S, line %d:\n"
        loc.loc_begin.Lexing.pos_fname
        loc.loc_begin.Lexing.pos_lnum;
      Printf.eprintf "Error: %s\n%!" s;
      exit 2)

let warning loc =
  Printf.kprintf (fun s ->
      Printf.eprintf "File %S, line %d:\n"
        loc.loc_begin.Lexing.pos_fname
        loc.loc_begin.Lexing.pos_lnum;
      Printf.eprintf "Warning: %s\n%!" s
    )

let ocp2_raise loc name v =
  raise (OCPExn (loc, name, v))

let ocp2_raise_format loc name fmt =
  Printf.kprintf (fun s -> ocp2_raise loc name (vstring s)) fmt

let raise_type_error loc prim_name arg_num type_expected value_received =
  ocp2_raise loc "type-error"
                 (VTuple [vstring prim_name;
                         VInt arg_num;
                         vstring type_expected;
                         value_received])

let raise_bad_arity loc prim_name nargs_expected args =
  raise_type_error loc prim_name nargs_expected "arity-expected" (VTuple args)

let prim_print loc ctx config args =
  let b = Buffer.create 111 in
  List.iter (fun arg ->
      BuildValue.bprint_value b "" arg;
    ) args;
  Printf.eprintf "%s\n%!" (Buffer.contents b);
  BuildValue.unit

let prim_raise loc ctx config args =
  match args with
  | [VString (name,_); v] ->
    raise (OCPExn (loc, name, v))
  | _ ->
    raise_bad_arity loc "raise(string,any)" 2 args

module Init(S: sig

    type context

  end) = struct

  let primitives = ref StringMap.empty

  let add_primitive s help
      ( f : location -> S.context -> config -> value list -> value)
    =
    let f loc ctx config args =
      try
        f loc ctx config args
      with
      | OCPExn _ as e -> raise e
      | e ->
        warning loc "exception raised by primitive %S" s;
        raise e
    in
    primitives := StringMap.add s (f, help) !primitives


  let apply_fun f loc ctx config args =
    match f with
    | VFunction f -> f loc args
    | VPrim name ->
       let (f, _) =
         try
           StringMap.find name !primitives
         with Not_found ->
           fatal_error loc "primitive %S not available" name
       in
       f loc ctx config args


let eprint_env indent env =
  let b = Buffer.create 1000 in
  BuildValue.bprint_env b indent env;
  Printf.eprintf "%s%!" (Buffer.contents b)

let varargs = -1

let _ =
  add_primitive "print" [ "Display its arguments: print(args)"  ] prim_print;

  add_primitive "message" [ "Print a message: print(STRING)"  ]
    (fun loc ctx config args ->
      match args with
      | [ VString (s,_) ] -> Printf.eprintf "%s\n%!" s; BuildValue.unit
      | _ -> raise_bad_arity loc "message(STRING)" 1 args
    );

  add_primitive "exit" [] (fun loc ctx config args ->
    match args with
    | [ VInt n ] -> exit n
    | _ -> exit 2
  );

  add_primitive "raise" [
    "Raise an exception with its argument: raise(\"exn\", arg);"
  ] prim_raise;

  add_primitive prim_identity_name [] (fun loc ctx config args ->
    match args with
    | [ v ] -> v
    | _ -> raise_bad_arity loc "identity(any)" 1 args
  );

  add_primitive prim_not_name [] (fun loc ctx config args ->
    match args with
    | [ VBool bool ] -> VBool (not bool)
    | [ VInt n ] -> VInt (-n)
    | _ -> raise_bad_arity loc "not(bool/int)" 1 args
  );

  add_primitive prim_or_name [] (fun loc ctx config args ->
    match args with
    | [ VBool bool1; VBool bool2 ] -> VBool (bool1 || bool2)
    | [ VInt n1; VInt n2 ] -> VInt (n1 lor n2)
    | _ -> raise_bad_arity loc "or(bool/int,bool/int)" 2 args
  );

  add_primitive prim_and_name [] (fun loc ctx config args ->
    match args with
    | [ VBool bool1; VBool bool2 ] -> VBool (bool1 && bool2)
    | [ VInt n1; VInt n2 ] -> VInt (n1 land n2)
    | _ -> raise_bad_arity loc "and(bool/int,bool/int)" 2 args
  );

  add_primitive prim_xor_name [] (fun loc ctx config args ->
    match args with
    | [ VInt n1; VInt n2 ] -> VInt (n1 lxor n2)
    | _ -> raise_bad_arity loc "xor(bool,bool)" 2 args
  );

  add_primitive prim_add_name [] (fun loc ctx config args ->
    match args with
    | [ VInt n1; VInt n2 ] -> VInt (n1 + n2)
    | [ VList l1; VList l2 ] -> VList (l1 @ l2)
    | [ VString (s1,kind); VString (s2,_) ] -> VString (s1 ^ s2, kind)
    | [ VString (s, kind); v ] ->
      VString (s ^ (BuildValue.string_of_value v), kind)
    | [ VObject env1; VObject env2 ] ->
      VObject (StringMap.fold (fun s v env ->
        BuildValue.set env s v
      ) env2.env env1)
    | _ -> raise_bad_arity loc "add(any,any)" 2 args
  );

  add_primitive prim_sub_name [] (fun loc ctx config args ->
    match args with
    | [ VInt n1; VInt n2 ] -> VInt (n1 - n2)
    | [ VObject { env = env }; VString (s,_) ] ->
      VObject { env = StringMap.remove s env }
    | [ VObject { env = env }; VList list ] ->
      let env = List.fold_left (fun env v ->
        match v with
        | VString (s,_) -> StringMap.remove s env
        | _ ->
          raise_type_error loc "sub(object,list)" 2 "string list" (VList list)
      ) env list in
      VObject { env }
    | _ -> raise_bad_arity loc "sub(int,int)" 2 args
  );

  (* We could use [ "n1"; "n2" ] * { x = 1 } as
     [ "n1", { x = 1 }; "n2", { x = 2 }]
  *)
  add_primitive prim_mul_name [] (fun loc ctx config args ->
    match args with
    | [ VInt n1; VInt n2 ] -> VInt (n1 * n2)
    | _ -> raise_bad_arity loc "mul(int,int)" 2 args
  );

  add_primitive prim_div_name [] (fun loc ctx config args ->
    match args with
    | [ VInt n1; VInt n2 ] ->
      if n2 = 0 then ocp2_raise loc "div-by-zero" (VTuple args);
      VInt (n1 / n2)
    | _ -> raise_bad_arity loc "div(int,int)" 2 args
  );

  add_primitive prim_mod_name [] (fun loc ctx config args ->
    match args with
    | [ VInt n1; VInt n2 ] ->
      if n2 = 0 then ocp2_raise loc "mod-by-zero" (VTuple args);
      VInt (n1 mod n2)
    | _ -> raise_bad_arity loc "mod(int,int)" 2 args
  );

  add_primitive prim_lsl_name [] (fun loc ctx config args ->
    match args with
    | [ VInt n1; VInt n2 ] -> VInt (n1 lsl n2)
    | _ -> raise_bad_arity loc "lsl(int,int)" 2 args
  );

  add_primitive prim_mod_name [] (fun loc ctx config args ->
    match args with
    | [ VInt n1; VInt n2 ] -> VInt (n1 lsr n2)
    | _ -> raise_bad_arity loc "lsr(int,int)" 2 args
  );

  add_primitive prim_lessthan_name [] (fun loc ctx config args ->
    match args with
    | [ v1; v2 ] ->
      VBool (BuildValue.compare_values v1 v2 < 0)
    | _ -> raise_bad_arity loc "lessthan(any,any)" 2 args
  );

  add_primitive prim_lessequal_name [] (fun loc ctx config args ->
    match args with
    | [ v1; v2 ] ->
      VBool (BuildValue.compare_values v1 v2 <= 0)
    | _ -> raise_bad_arity loc "lessequal(any,any)" 2 args
  );

  add_primitive prim_greaterthan_name [] (fun loc ctx config args ->
    match args with
    | [ v1; v2 ] ->
      VBool (BuildValue.compare_values v1 v2 > 0)
    | _ -> raise_bad_arity loc "greaterthan(any,any)" 2 args
  );

  add_primitive prim_greaterequal_name [] (fun loc ctx config args ->
    match args with
    | [ v1; v2 ] ->
      VBool (BuildValue.compare_values v1 v2 >= 0)
    | _ -> raise_bad_arity loc "greaterequal(any,any)" 2 args
  );

  add_primitive prim_equal_name [] (fun loc ctx config args ->
    match args with
    | [ v1; v2 ] ->
      VBool (BuildValue.compare_values v1 v2 = 0)
    | _ -> raise_bad_arity loc "equal(any,any)" 2 args
  );

  add_primitive prim_notequal_name [] (fun loc ctx config args ->
    match args with
    | [ v1; v2 ] ->
      VBool (BuildValue.compare_values v1 v2 <> 0)
    | _ -> raise_bad_arity loc "notequal(any,any)" 2 args
  );

  (* ------------------------------------------------------------

     Pervasives module

     ------------------------------------------------------------*)

  (* This primitive should be used as:
     List = module( "List" );
     to load a set of functions from a file. Since we have to interprete
     the content of the file, it can only be implemented in Interp.ml
  *)



  add_primitive "module" [] (fun loc ctx config args ->
    let modname, required_version =
      match args with
      | [ VString (modname,_) ] -> modname, None
      | [ VString (modname,_); VString (version,_) ] ->
        let version = Versioning.version_of_string version in
        modname, Some version
      | _ -> raise_bad_arity loc "module(string)" 1 args
    in
    try
      let (module_desc, version) =
        StringMap.find modname config.config_state.cfs_modules in
      begin
        match required_version with
        | None -> ()
        | Some required_version ->
          if Versioning.compare required_version version > 0 then
            ocp2_raise_format loc
              "bad-version"
              "module %S requires version %s, but version %s found"
              modname
              (Versioning.string_of_version required_version)
              (Versioning.string_of_version version)
      end;
      match !module_desc with
      | Computed v -> v
      | Computing ->
          ocp2_raise_format loc "infinite-dependency"
            "in module %S" modname
      | Declared v ->
        let v =
          match v with
          | VFun f -> apply_fun f loc ctx config []
          | _ -> v
        in
        module_desc := Computed v;
        v
    with Not_found ->
      ocp2_raise_format loc "not-found" "could not find module %S" modname
  );

  add_primitive "provides" [] (fun loc ctx config args ->
    match args with
    | [ VString (s,_); VString (version,_); value ] ->
      let version = Versioning.version_of_string version in
      begin
        try
          let (_old_value, old_version) =
            StringMap.find s config.config_state.cfs_modules in
          if Versioning.compare old_version version >= 0 then
            raise Not_found
        with Not_found ->
          config.config_state.cfs_modules <-
            StringMap.add s (ref (Declared value), version)
            config.config_state.cfs_modules
      end;
      VList []
    | _ -> raise_bad_arity loc "provides(string, version, value)" 2 args
  );


  (* Specific to OCaml *)
  add_primitive "packer" [
    "pack(string[,pack_env], list-of-strings)"
  ] (fun loc ctx config args ->
    Printf.eprintf
      "Warning: function 'packer' is deprecated. Use 'OCaml.pack' instead\n%!";
    let packmodname, pack_env, files =
      match args with
      | [VString (packmodname,_); files] ->
        (packmodname, BuildValue.empty_env, files)
      | [VString (packmodname,_); VObject pack_env; files] ->
        (packmodname, pack_env, files)
      | _ ->
        raise_bad_arity loc "pack(name, files)" 2 args
    in
    let files = BuildValue.prop_list files in
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

  add_primitive "value_length"
    [
      "[length(v)] returns the length of [v], if it is a string, list or tuple"
    ]
    (fun loc ctx config args ->
      match args with
      | [ VString (s, _) ] -> VInt (String.length s)
      | [ VList l | VTuple l ] -> VInt (List.length l)
      | _ ->
        raise_bad_arity loc "length([string|list|tuple])" 1 args
    );

  add_primitive "value_type"
    [
      "[typeof(v)] returns the type of [v], as a string."
    ]
    (fun loc ctx config args ->
      match args with
      | [ arg ] ->
        vstring (match arg with
        | VString _ -> "string"
        | VList _  ->  "list"
        | VTuple _ -> "tuple"
        | VObject _ -> "object"
        | VBool _ -> "bool"
        | VInt _ -> "int"
        | VFun (VFunction _) -> "function"
        | VFun (VPrim _) -> "prim"
        )
      | _ ->
        raise_bad_arity loc "version(string)" 1 args
    );

  add_primitive "version"
    [ "version(STRING) translates its argument into a version,";
      "so that later comparisons will use version comparison.";]
    (fun loc ctx config args ->
      match args with
      | [ VString (s, _) ] -> VString (s, StringVersion)
      | _ ->
        raise_bad_arity loc "version(string)" 1 args
    );

  add_primitive "SRCDIR" []
    (fun loc ctx config args ->
      match args with
      | [ VString (s, _) ] ->
        VString ("%{" ^ s ^ "_FULL_SRC_DIR}%", StringVersion)
      | [ VString (pk, _); VString (file,_) ] ->
        VString ("%{" ^ pk ^ "_FULL_SRC_DIR}%/" ^ file, StringVersion)
      | _ ->
        raise_bad_arity loc "SRCDIR(string[,file])" 1 args
    );

  add_primitive "DSTDIR" []
    (fun loc ctx config args ->
      match args with
      | [ VString (s, _) ] ->
        VString ("%{" ^ s ^ "_FULL_DST_DIR}%", StringVersion)
      | [ VString (pk, _); VString (file,_) ] ->
        VString ("%{" ^ pk ^ "_FULL_DST_DIR}%/" ^ file, StringVersion)
      | _ ->
        raise_bad_arity loc "DSTDIR(string[,file])" 1 args
    );

  add_primitive "store_set" [
    "[store_set(name,v)] associates [name] with [v] in the";
    "semi-persistent store."
  ]
(fun loc ctx config args ->
      match args with
      | [ VString (name, _); v ] ->
        config.config_state.cfs_store <-
          StringMap.add name v config.config_state.cfs_store;
        BuildValue.unit
      | _ ->
        raise_bad_arity loc "store_set(string,v)" 1 args
    );
  add_primitive "store_get" [
    "[store_get(name)] retrieves the value associateed with [name] in the";
    "semi-persistent store."
  ]
(fun loc ctx config args ->
      match args with
      | [ VString (name, _) ] ->
          begin try
                  StringMap.find name config.config_state.cfs_store
            with Not_found ->
              raise (OCPExn (loc, "not-found",
                 vstring name))
          end
      | _ ->
        raise_bad_arity loc "store_get(string)" 1 args
    );

  (* ------------------------------------------------------------

     List module

     ------------------------------------------------------------*)

  let failure loc msg =
    raise (OCPExn (loc, "failure", vstring msg))
  in
  add_primitive "List_mem" []
    (fun loc ctx config args ->
      match args with
      | [ ele; VList list ] ->
        VBool (List.mem ele list )
      | _ ->
        raise_bad_arity loc "List.mem(ele, list)" 2 args
    );

  add_primitive "List_tail" []
    (fun loc ctx config args ->
      match args with
      | [ VList list ] -> begin
        match list with
        | [] -> failure loc "List.tail on empty list"
        | _ :: list -> VList list
      end
      | _ ->
        raise_bad_arity loc "List.tail(list)" 1 args
    );

  add_primitive "List_map" []
    (fun loc ctx config args ->
      match args with
      | [ VFun f; VList list ] ->
        VList (List.map (fun v -> apply_fun f loc ctx config [v]) list)
      | [ VList list; VFun f ] ->
        VList (List.map (fun v -> apply_fun f loc ctx config [v]) list)
      | _ ->
        raise_bad_arity loc "List.map(function, list)" 2 args
    );

  add_primitive "List_flatten" []
    (fun loc ctx config args ->
      match args with
      | [ VList list ] ->
        VList (List.flatten
                 (List.map (function
                 | VList l -> l
                 | v -> [v]) list
                 ))
      | _ ->
        raise_bad_arity loc "List.flatten(list)" 1 args
    );

  add_primitive "List_fold_left" []
    (fun loc ctx config args ->
      match args with
      | [ VFun f; acc; VList list ] ->
        List.fold_left (fun acc ele ->
            apply_fun f loc ctx config [acc; ele]
          ) acc list
      | _ ->
        raise_bad_arity loc "List.fold_left(f, acc, list)" 3 args
    );

  (* ------------------------------------------------------------

     String module

     ------------------------------------------------------------*)

  add_primitive "String_mem" []
    (fun loc ctx config args ->
      match args with
      | [ VString (ele,_); VString (list,_) ] ->
        VBool (try ignore (OcpString.find ele list); true
          with Not_found -> false )
      | _ ->
        raise_bad_arity loc "String.mem(ele, list)" 2 args
    );

  add_primitive "String_concat" []
    (fun loc ctx config args ->
      let list, sep =
        match args with
        | [ VList list ] -> list, ""
        | [ VList list; VString (sep,_) ] -> list, sep
        | [ VString (sep,_); VList list  ] -> list, sep
        | _ ->
          raise_bad_arity loc "String.concat(list[, sep])" 2 args
      in
      let list = List.map (function
        | VString (s,_) -> s
        | _ ->
          raise_bad_arity loc "String.concat(list of strings[, sep])" 2 args
      ) list in
      let s = String.concat sep list in
      vstring s
    );

  add_primitive "String_capitalize" []
    (fun loc ctx config args ->
      match args with
      | [ VString (ele,_) ] -> vstring (String.capitalize ele)
      | _ ->
        raise_bad_arity loc "String.capitalize(ele)" 1 args
    );

  add_primitive "String_write_file" []
    (fun loc ctx config args ->
      match args with
      | [ VString (filename,_); VString (content, _) ] ->
         FileString.write_file filename content;
         VBool true
      | _ ->
        raise_bad_arity loc "String.write_file(filename, content)" 2 args
    );

  add_primitive "String_read_file" []
    (fun loc ctx config args ->
      match args with
      | [ VString (filename,_) ] -> vstring (FileString.read_file filename)
      | _ ->
        raise_bad_arity loc "String.read_file(filename)" 1 args
    );


  (* ------------------------------------------------------------

     Sys module

     ------------------------------------------------------------*)

  (* Only since 1.99.18-beta *)
  add_primitive "Sys_file_exists" []
    (fun loc ctx config args ->
      match args with
      | [ VString (file,_) ] ->
        VBool (Sys.file_exists file)
      | _ ->
        raise_bad_arity loc "Sys.file_exists(file)" 1 args
    );

  (* Warning: translate only '.', '*', and '?' to Str *)
  let regexp_shell_to_str regexp =
    let len = String.length regexp in
    let b = Buffer.create len in
    for i = 0 to len -1 do
      match regexp.[i] with
      | '.' -> Buffer.add_string b "\\."
      | '*' -> Buffer.add_string b ".*"
      | '?' -> Buffer.add_char b '.'
      | c -> Buffer.add_char b c
    done;
    Buffer.add_string b "$";
    Buffer.contents b
  in

  let readdir loc config dirname =
    let dirname =
      if Filename.is_relative dirname then
        Filename.concat config.config_dirname dirname
      else dirname
    in
    try
      Sys.readdir dirname
    with e ->
      fatal_error loc "Sys.readdir failed on %S" dirname
  in
  add_primitive "Sys_readdir" []
    (fun loc ctx config args ->
      match args with
      | [ VString (dir,_) ] ->
        let files = readdir loc config dir in
        let files = Array.to_list files in
        VList (List.map (fun s -> vstring s) files)
      | [ VString (dir,_); VString (regexp,_) ] ->
        let files = readdir loc config dir in
        let files = Array.to_list files in
        let regexp = regexp_shell_to_str regexp in
        let regexp = Str.regexp regexp in
        let files = List.filter (fun file ->
          Str.string_match regexp file 0) files in
        VList (List.map (fun s -> vstring s) files)
      | _ ->
        raise_bad_arity loc "Sys.readdir(dir)" 1 args
    );

  (*

    add_function "byte_exe" [] (fun envs _env ->
    let p = BuildValue.get_local_string envs "p" in
    let s = Printf.sprintf "%%{%s_FULL_DST_DIR}%%/%s.byte" p p in
    VString s
    );

    add_function "asm_exe" [] (fun envs _env ->
    let p = BuildValue.get_local_string envs "p" in
    let s = Printf.sprintf "%%{%s_FULL_DST_DIR}%%/%s.asm" p p in
    VString s
    );

  *)

  add_primitive "String_split" []
    (fun loc ctx config args ->
      match args with
      | [ VString (s,_); VString (sep,_) ] ->
        if String.length sep <> 1 then
          failure loc "String_split with separator length <> 1";
        VList (List.map vstring (OcpString.split s sep.[0]))
      | _ ->
        raise_bad_arity loc "String_split(s,sep)" 2 args
    );

  add_primitive "String_split_simplify" []
    (fun loc ctx config args ->
      match args with
      | [ VString (s,_); VString (sep,_) ] ->
        if String.length sep <> 1 then
          failure loc "String_split_simplify with separator length <> 1";
        VList (List.map vstring (OcpString.split_simplify s sep.[0]))
      | _ ->
        raise_bad_arity loc "String_split(s,sep)" 2 args
    );

  add_primitive "String_subst_suffix" []
    (fun loc ctx config args ->
      match args with
      | [ VString (s,_) as v;
          VString (old_suffix,_); VString (new_suffix,_) ] ->
        if Filename.check_suffix s old_suffix then
          vstring (Filename.chop_suffix s old_suffix ^ new_suffix)
        else v
      | [ VList list;
          VString (old_suffix,_); VString (new_suffix,_) ] ->
        VList (List.map (function
        | VString (s,_) as v ->
          if Filename.check_suffix s old_suffix then
            vstring (Filename.chop_suffix s old_suffix ^ new_suffix)
          else v
        | v -> v) list)
      | _ ->
        raise_bad_arity loc "String_subst_suffix(string|list,src,dst)" 3 args
    );

  add_primitive "String_filter_by_suffix" []
    (fun loc ctx config args ->
      match args with
      | [ VList list; VString (suffix,_) ] ->
        VList (List.filter (function
        | VString (s,_) -> Filename.check_suffix s suffix
        | v -> false) list)
      | _ ->
        raise_bad_arity loc "String_filter_by_suffix(list,suffix)" 2 args
    );

  add_primitive "String_subst" []
    (fun loc ctx config args ->
      match args with
      | [ VString (s,_); VList assocs ] ->
        let rec iter assocs id =
          match assocs with
          | [] -> failure loc (Printf.sprintf "String_subst cannot subst %S" id)
          | VTuple [ VString (s, _); rep ] :: _ when s = id ->
            BuildValue.string_of_value rep
          | _ :: assocs -> iter assocs id
        in
        let b = Buffer.create (String.length s) in
        Buffer.add_substitute b (iter assocs) s;
        vstring (Buffer.contents b)
      | _ ->
        raise_bad_arity loc "String_subst(s,assoc)" 2 args
    );

  add_primitive "Features_get"
                [ "Query the map of user-selected features" ]
                (fun loc ctx config args ->
                  match args with
                    [ VString (name, _); VBool default ] ->
                    queried_features := StringMap.add name default !queried_features;
                    begin
                      try
                        BuildValue.get [!features] name
                      with Var_not_found _ ->
                        let env = BuildValue.empty_env in
                        let env = BuildValue.set_bool env "enabled" default in
                        let env = BuildValue.set_bool env "user" false in
                        VObject env
                    end
                  | _ -> raise_bad_arity loc "Features.get(string,bool)" 2 args
                );

  add_primitive "Features_enable"
                [ "Enable/disable a feature" ]
                (fun loc ctx config args ->
                  match args with
                  | [ VString (name, _); VBool enable ] ->
                    if enable then
                      with_feature name
                    else
                      with_feature name;
                    BuildValue.unit
                  | [ VString (name, _) ] ->
                     with_feature name;
                     BuildValue.unit
                  | _ ->
                     raise_bad_arity loc "Features.enable(string,bool)" 2 args
                );

  add_primitive "Features_with"
                [ "Set a feature" ]
                (fun loc ctx config args ->
                  match args with
                  | [ VString (name, _); (VString (value,_)) ] ->
                     with_feature_value name value;
                     BuildValue.unit
                  | [ VString (name, _) ] ->
                     with_feature name;
                     BuildValue.unit
                  | _ -> raise_bad_arity loc "Features.with(string,string)" 2 args
                );

  add_primitive "Features_without"
                [ "Unset a feature" ]
                (fun loc ctx config args ->
                  match args with
                  | [ VString (name, _) ] ->
                     without_feature name;
                     BuildValue.unit
                  | _ -> raise_bad_arity loc "Features.without(string)" 1 args
                );

  ()

let primitives_help () =
  StringMap.map (fun (_,h) -> h) !primitives



end

let queried_features () = !queried_features
