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

module TYPES = struct


  type location = {
    loc_begin : Lexing.position;
    loc_end : Lexing.position;
  }

  type string_kind =
  | StringRaw
  | StringVersion


  type env = { env : value StringMap.t }
  and value =
  | VList of value list
  | VObject of env
  | VString of string * string_kind
  | VTuple of value list
  | VBool of bool
  | VInt of int
  | VFun of functional_value

  and functional_value =
  | VFunction of (location -> value list -> value)
  | VPrim of string

(* Just for compatibility: a plist is morally a
   VList of VTuple (VString * VObject) *)
  type plist = value
  type prop_list = (string * env) list

  type 'a source_option = {
    get : env list -> 'a;
    set : 'a -> unit;
  }
  exception Var_not_found of string
  exception NotAPropertyList

  (* To avoid dealing with dependencies, modules can be declared lazily
     by provides("Mod", function(){...}), in which case the function will
     only be executed on demand. When doing so, we set the module to
     Computing to avoid a recursion. *)
  type module_desc =
  | Declared of value
  | Computing
  | Computed of value

  type config_state = {
    mutable cfs_modules : (module_desc ref * Versioning.version) StringMap.t;
    mutable cfs_store : value StringMap.t;
  }

  (* The configuration at a package definition site *)
  type config = {
    config_env : env;
    config_state : config_state;
    config_dirname : string;
    config_filename : string;
    config_filenames : (string * Digest.t option) list;
  }

end

open TYPES



let iter_env f env =
  StringMap.iter f env.env

let rec
    (*
bprint_plist b indent list =
  match list with
    [] -> Printf.bprintf b "%s[]\n" indent
  | list ->
    Printf.bprintf b "%s[\n" indent;
    List.iter (fun (s, env) ->
      Printf.bprintf b "%s  %S\n" indent s;
      if env <> BuildValue.empty_env then begin
        Printf.bprintf b "%s  (\n" indent;
        bprint_env b (indent ^ "  ") env;
        Printf.bprintf b "%s  )\n" indent;
      end
    ) list;
    Printf.bprintf b "%s]\n" indent;
    ()

      and *)
    bprint_value b indent v =
  match v with
  | VString (s,_) -> Printf.bprintf b "%S" s
  | VBool bool -> Printf.bprintf b "%b" bool
  | VInt int -> Printf.bprintf b "%d" int
  | VTuple [] -> assert false
  | VTuple (v1 :: list) ->
    bprint_value b indent v1;
    List.iter (fun v2 ->
        Printf.bprintf b ", ";
        bprint_value b indent v2) list
  | VObject env ->
    Printf.bprintf b "{\n";
    bprint_env b (indent^"  ") env;
    Printf.bprintf b "%s}" indent
  | VList [] ->
    Printf.bprintf b "[]"
  | VList list ->
    Printf.bprintf b "[\n";
    List.iter (fun v ->
      Printf.bprintf b "%s" indent;
      bprint_value b indent v;
      Printf.bprintf b "\n") list;
    Printf.bprintf b "%s]" indent
  | VFun (VFunction _) -> Printf.bprintf b "function(...){...}"
  | VFun (VPrim s) -> Printf.bprintf b "primitive(%S)" s

and bprint_env b indent env =
  iter_env (fun var v ->
    Printf.bprintf b "%s%s -> " indent var;
    bprint_value b (indent^"  ") v;
    Printf.bprintf b "\n"
  ) env

let string_of_value v =
  let b = Buffer.create 1000 in
  bprint_value b "" v;
  Buffer.contents b

let string_of_env v =
  let b = Buffer.create 1000 in
  bprint_env b "" v;
  Buffer.contents b

let empty_env = { env = StringMap.empty }
let global_env = ref StringMap.empty
let set_global name v =
  global_env := StringMap.add name v !global_env

let set env name v = { env = StringMap.add name v env.env }
let rec get_local envs name =
  match envs with
    [] ->
      (*    Printf.eprintf "get_local %S failed\n%!" name; *)
    raise (Var_not_found name)
  | env :: envs ->
    try
      StringMap.find name env.env
    with Not_found ->
      get_local envs name

let get_global name =
      try
      StringMap.find name !global_env
    with Not_found ->
(*      Printf.eprintf "get_global %S failed\n%!" name; *)
      raise (Var_not_found name)

let get envs name =
  try get_local envs name
  with Var_not_found _ -> get_global name

let prop_list v =
  match v with
  | VString (s,_) -> [s, empty_env]
  | VList list ->
    List.map (fun v ->
      match v with
      | VString (s,_) -> s, empty_env
      | VTuple [VString (s,_); VObject env] -> s, env
      | _ ->
        Printf.eprintf "Not a property list element: %s\n%!"
          (string_of_value v);
        raise NotAPropertyList
    ) list
  | _ ->
    Printf.eprintf "Not a property list: %s\n%!"
      (string_of_value v);
    raise NotAPropertyList

let value list =
  VList (List.map (fun (s,env) ->
    if env == empty_env then VString (s, StringRaw) else
      VTuple [VString (s, StringRaw); VObject env]
  ) list)

let plist_of_bool b = VBool b
let bool_of_plist v =
  match v with
  | VBool bool -> bool
  | VList [] -> false
  | _ -> true

let plist_of_strings strings =
  VList (List.map (fun s -> VString (s, StringRaw)) strings)
let strings_of_plist list =
  List.map (fun (s,_) -> s) (prop_list list)

let plist_of_string_option option =
  match option with
  | None -> VList []
  | Some s -> VString (s, StringRaw)

let string_option_of_plist list = match list with
  | VList [] -> None
  | _ -> Some (String.concat " " (strings_of_plist list))

let plist_of_string s = VString (s, StringRaw)
let plist_of_version s = VString (s, StringVersion)
let string_of_plist list = (String.concat " " (strings_of_plist list))

let plist_of_path s = VString (s, StringRaw)
let path_of_plist list = String.concat "/" (strings_of_plist list)

let set_bool env name v = set env name (plist_of_bool v)
let get_bool env name = bool_of_plist (get env name)
let get_local_bool env name = bool_of_plist (get_local env name)

let set_strings env name v = set env name (plist_of_strings v)
let get_strings env name = strings_of_plist (get env name)
let get_local_strings env name = strings_of_plist (get_local env name)

(*let set_version env name v = set env name (VString (v, StringVersion))*)
let set_string env name v = set env name (plist_of_string v)
let get_string env name = string_of_plist (get env name)
let get_local_string env name = string_of_plist (get_local env name)

let set_string_option env name v = set env name (plist_of_string_option v)
let get_string_option env name = string_option_of_plist (get env name)
let get_local_string_option env name = string_option_of_plist (get_local env name)

let set_path env name v = set env name (plist_of_path v)
let get_path env name = path_of_plist (get env name)
let get_local_path env name = path_of_plist (get_local env name)

let get_with_default_fun f =
  fun env name default -> try f env name with Var_not_found _ -> default

let get_with_default = get_with_default_fun get

let get_local_with_default = get_with_default_fun get_local
let get_local_prop_list env name = prop_list (get_local env name)
let get_local_prop_list_with_default = get_with_default_fun get_local_prop_list

let get_bool_with_default = get_with_default_fun get_bool
let get_strings_with_default = get_with_default_fun get_strings
let get_string_with_default = get_with_default_fun get_string
let get_string_option_with_default = get_with_default_fun get_string_option
let get_path_with_default = get_with_default_fun get_path

let get_local_bool_with_default = get_with_default_fun get_local_bool
let get_local_strings_with_default = get_with_default_fun get_local_strings
let get_local_string_with_default = get_with_default_fun get_local_string
let get_local_string_option_with_default = get_with_default_fun get_local_string_option
let get_local_path_with_default = get_with_default_fun get_local_path

let is_already_installed options =
  get_bool_with_default options "generated" false
  || get_bool_with_default options "installed" false

let new_option name v =
  set_global name v;
  {
    get = (fun env -> get env name);
    set = (fun v -> set_global name v);
  }

let new_bool_option name v =
  set_global name (plist_of_bool v);
  {
    get = (fun env -> get_bool env name);
    set = (fun v -> set_global name (plist_of_bool v));
  }

let new_strings_option name v =
  set_global name (plist_of_strings v);
  {
    get = (fun env -> get_strings env name);
    set = (fun v -> set_global name (plist_of_strings v));
  }

let new_string_option name v =
  set_global name (plist_of_string v);
  {
    get = (fun env -> get_string env name);
    set = (fun v -> set_global name (plist_of_string v));
  }

let new_version_option name v =
  set_global name (plist_of_version v);
  {
    get = (fun env -> get_string env name);
    set = (fun v -> set_global name (plist_of_version v));
  }

    (*
let new_path_option name v =
  set_global name (plist_of_path v);
  {
    get = (fun env -> get_path env name);
    set = (fun v -> set_global name (plist_of_path v));
  }
    *)

let empty_config_state () =
  { cfs_modules = StringMap.empty;
    cfs_store = StringMap.empty; }

let empty_config () = {
  config_env = empty_env;
  config_state = empty_config_state ();
  config_dirname = "";
  config_filename = "";
  config_filenames = [];
}


let config_get config name =
  get [config.config_env] name
let config_set config name v =
  { config with config_env = set config.config_env name v }

let unit = VObject empty_env

let noloc pos_fname =
  let pos =
    let open Lexing in
    {
      pos_fname;
      pos_lnum = -1;
      pos_bol = 0;
      pos_cnum = 0;
    } in
  {
    loc_begin = pos;
    loc_end = pos;
  }

let string_of_location loc =
  let pos = loc.loc_begin in
  let open Lexing in
  Printf.sprintf "File %S, line %d, char %d"
    pos.pos_fname pos.pos_lnum pos.pos_cnum


let rec set_deep_field env fields value =
  match fields with
  | [] -> assert false
  | [field] -> set env field value
  | field :: fields ->
    let value =
      let env =
        try
          match get_local [env] field with
          | VObject env -> env
          | v ->
            Printf.kprintf failwith
              "BuildValue.set_deep_field: field %S not an object but %s" field
              (string_of_value v)
        with Var_not_found _ -> empty_env
      in
      set_deep_field env fields value
    in
    set env field (VObject value)

let set_deep_field env fields value =
  try
    set_deep_field env fields value
  with exn ->
    Printf.eprintf
      "BuildValue.set_deep_field:\nenv:%s\nfields: [%s]\nvalue:%s\n%!"
      (string_of_env env)
      (String.concat " ; " fields)
      (string_of_value value)
    ;
    raise exn

let new_object assoc =
    let env = List.fold_left (fun env (label, v) ->
      StringMap.add label v env
    ) StringMap.empty assoc
    in
    VObject { env }



let compare_versions s1 s2 =
  Versioning.compare
    (Versioning.version_of_string s1)  (Versioning.version_of_string s2)

let revassoc_of_env env =
  StringMap.fold (fun n v (nn, vv) ->
    (n :: nn, v :: vv)
  ) env ([],[])

let fold f env x = StringMap.fold f env.env x

let rec compare_values e1 e2 =
  match e1, e2 with

  (* comparison of strings *)
  | VString (s1, StringVersion), VString (s2,_)
  | VString (s1, StringVersion), VList [ VString (s2,_) ]
  | VList [VString (s1, StringVersion)], VString (s2,_)
  | VString (s1,_), VString (s2, StringVersion)
  | VString (s1,_), VList [ VString (s2, StringVersion) ]
  | VList [VString (s1,_)], VString (s2, StringVersion)
    -> compare_versions s1 s2
  | VString (s1,_), VString (s2,_)
  | VString (s1,_), VList [ VString (s2,_) ]
  | VList [VString (s1,_)], VString (s2,_)
    -> Pervasives.compare s1 s2

  (* comparison of lists *)
  | VList [], VList [] -> 0
  | VList [], VList _ -> -1
  | VList _, VList [] -> 1
  | VList (h1::t1), VList (h2::t2) ->
    (match compare_values h1 h2 with
     | 0 -> compare_values (VList t1) (VList t2)
     | v -> v)
  (* comparison of tuples. Must be of the same size. *)
  | VTuple t1, VTuple t2 when List.length t1 = List.length t2 ->
    compare_values (VList t1) (VList t2)

  (* comparison of ints *)
  | VInt n1, VInt n2 -> Pervasives.compare n1 n2

  (* comparison of bools *)
  | VBool n1, VBool n2 -> Pervasives.compare n1 n2

  (* comparison of objects: objects can only be compared if they have
     exactly the same fields. Fields are then sorted by names and
     compared. *)
  | VObject o1, VObject o2 ->
    let n1, t1 = revassoc_of_env o1.env in
    let n2, t2 = revassoc_of_env o2.env in
    if n1 <> n2 then
      failwith "BuildValue.compare_values"
    else
      compare_values (VList (List.rev t1)) (VList (List.rev t2))

  | _ ->
    failwith "BuildValue.compare_values"
