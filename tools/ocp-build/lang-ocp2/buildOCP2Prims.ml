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
open BuildValue.Types
open BuildOCP2Tree

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


let raise_type_error loc prim_name arg_num type_expected value_received =
  raise (OCPExn (loc, "type-error",
                 VTuple [VString prim_name;
                         VInt arg_num;
                         VString type_expected;
                         value_received]))

let raise_bad_arity loc prim_name nargs_expected args =
  raise_type_error loc prim_name nargs_expected "arity-expected" (VTuple args)

let prim_print loc ctx config args =
  let b = Buffer.create 111 in
  List.iter (fun arg ->
      BuildValue.bprint_value b "" arg;
    ) args;
  Printf.eprintf "%s\n%!" (Buffer.contents b);
  BuildValue.value []

let prim_raise loc ctx config args =
  match args with
  | [VString name; v] ->
    raise (OCPExn (loc, name, v))
  | _ ->
    raise_bad_arity loc "raise(string,any)" 2 args

let ocp2_raise loc name v =
  raise (OCPExn (loc, name, v))

module Init(S: sig

    type context

    val define_package :
      location ->
      context ->
      config ->
      name:string ->
      kind:string ->
      unit

    val filesubst : (string * env list) StringSubst.M.subst

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

let eprint_env indent env =
  let b = Buffer.create 1000 in
  BuildValue.bprint_env b indent env;
  Printf.eprintf "%s%!" (Buffer.contents b)

(*
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

*)
    (*
          if arity >= 0 &&
             List.length args <> arity then
            error loc "%s has arity %d" name arity;
*)

let varargs = -1

let _ =
  add_primitive "print" [ "Display its arguments: print(args)"  ] prim_print;

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
    | [ VString s1; VString s2 ] -> VString (s1 ^ s2)
    | [ VString s; v ] -> VString (s ^ (BuildValue.string_of_value v))
    | [ VObject env1; VObject env2 ] ->
      VObject (StringMap.fold (fun s v env ->
        BuildValue.set env s v
      ) env2.env env1)
    | _ -> raise_bad_arity loc "add(any,any)" 2 args
  );

  add_primitive prim_sub_name [] (fun loc ctx config args ->
    match args with
    | [ VInt n1; VInt n2 ] -> VInt (n1 - n2)
    | [ VObject { env = env }; VString s ] ->
      VObject { env = StringMap.remove s env }
    | [ VObject { env = env }; VList list ] ->
      let env = List.fold_left (fun env v ->
        match v with
        | VString s -> StringMap.remove s env
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
    | [ v1; v2 ] -> VBool (v1 < v2)
    | _ -> raise_bad_arity loc "lessthan(any,any)" 2 args
  );

  add_primitive prim_greaterthan_name [] (fun loc ctx config args ->
    match args with
    | [ v1; v2 ] -> VBool (v1 > v2)
    | _ -> raise_bad_arity loc "greaterthan(any,any)" 2 args
  );

  add_primitive prim_lessequal_name [] (fun loc ctx config args ->
    match args with
    | [ v1; v2 ] -> VBool (v1 <= v2)
    | _ -> raise_bad_arity loc "lessequal(any,any)" 2 args
  );

  add_primitive prim_greaterequal_name [] (fun loc ctx config args ->
    match args with
    | [ v1; v2 ] -> VBool (v1 >= v2)
    | _ -> raise_bad_arity loc "greaterequal(any,any)" 2 args
  );

  add_primitive prim_equal_name [] (fun loc ctx config args ->
    match args with
    | [ v1; v2 ] -> VBool (v1 = v2)
    | _ -> raise_bad_arity loc "equal(any,any)" 2 args
  );

  add_primitive prim_notequal_name [] (fun loc ctx config args ->
    match args with
    | [ v1; v2 ] -> VBool (v1 <> v2)
    | _ -> raise_bad_arity loc "notequal(any,any)" 2 args
  );

  (* This primitive should be used as:
     List = module( "List" );
     to load a set of functions from a file. Since we have to interprete
     the content of the file, it can only be implemented in Interp.ml
  *)
  add_primitive "module" [] (fun loc ctx config args ->
    let modname, required_version =
      match args with
      | [ VString modname ] -> modname, None
      | [ VString modname; VString version ] ->
        let version = Versioning.version_of_string version in
        modname, Some version
      | _ -> raise_bad_arity loc "module(string)" 1 args
    in
    try
      let (v, version) = StringMap.find modname !(config.config_modules) in
      begin
        match required_version with
        | None -> ()
        | Some required_version ->
          if Versioning.compare required_version version > 0 then
            fatal_error loc
              "module %S required to have version %s, but version %s found"
              modname
              (Versioning.string_of_version required_version)
              (Versioning.string_of_version version)
      end;
      v
    with Not_found ->
      fatal_error loc "could not find module %S" modname
  );

  add_primitive "provides" [] (fun loc ctx config args ->
    match args with
    | [ VString s; VString version; value ] ->
      let version = Versioning.version_of_string version in
      begin
        try
          let (_old_value, old_version) =
            StringMap.find s !(config.config_modules) in
          if Versioning.compare old_version version >= 0 then
            raise Not_found
        with Not_found ->
          config.config_modules :=
            StringMap.add s (value, version) !(config.config_modules)
      end;
      VList []
    | _ -> raise_bad_arity loc "provides(string, version, value)" 2 args
  );

  add_primitive "new_package" [
    "Create a new package: new_package(name, kind, ocaml)"
  ]
    (fun loc ctx config args ->
      match args with
      | [VString name; VString kind; VObject config_env] ->
        S.define_package loc ctx { config  with config_env } ~name ~kind;
        VList []
      | _ ->
        raise_bad_arity loc "new_package(string,string,object)" 3 args
    );


  (* Specific to OCaml *)
  add_primitive "packer" [
    "pack(string[,pack_env], list-of-strings)"
  ] (fun loc ctx config args ->
    let packmodname, pack_env, files =
      match args with
      | [VString packmodname; files] ->
        (packmodname, BuildValue.empty_env, files)
      | [VString packmodname; VObject pack_env; files] ->
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

  add_primitive "List_mem" []
    (fun loc ctx config args ->
      match args with
      | [ ele; VList list ] ->
        VBool (List.mem ele list )
      | _ ->
        raise_bad_arity loc "List.mem(ele, list)" 2 args
    );

  add_primitive "String_mem" []
    (fun loc ctx config args ->
      match args with
      | [ VString ele; VString list ] ->
        VBool (try ignore (OcpString.find ele list); true
          with Not_found -> false )
      | _ ->
        raise_bad_arity loc "String.mem(ele, list)" 2 args
    );

  (*
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
    VString s
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
    VString s
    );

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
    VList (List.map (fun s -> VString s) (OcpString.split s sep))
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
    VList (List.map (fun s -> VString s) (OcpString.split_simplify s sep))
    );

    let uniq_counter = ref 0 in
    add_function "uniq" [
    "Returns a uniq string, to be used as a uniq identifier";
    ] (fun _ _ ->
    incr uniq_counter;
    VString (Printf.sprintf ".id_%d" !uniq_counter));
    ()

  *)

  ()

let primitives_help () =
  StringMap.map (fun (_,h) -> h) !primitives



end
