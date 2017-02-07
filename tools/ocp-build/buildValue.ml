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

module Types = struct


  type location = {
    loc_begin : Lexing.position;
    loc_end : Lexing.position;
  }

  type env = { env : value StringMap.t }
  and value =
  | VList of value list
  | VObject of env
  | VString of string
  | VTuple of value list
  | VBool of bool
  | VInt of int
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

  (* The configuration at a package definition site *)
  type config = {
    config_env : env;
    config_modules : (value * Versioning.version) StringMap.t ref;
    config_dirname : string;
    config_filename : string;
    config_filenames : (string * Digest.t option) list;
  }

end

open Types



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
  | VString s -> Printf.bprintf b "%S" s
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
    Printf.bprintf b "}"
  | VList [] ->
    Printf.bprintf b "[]"
  | VList list ->
    Printf.bprintf b "[\n";
    List.iter (fun v ->
      Printf.bprintf b "%s" indent;
      bprint_value b indent v;
      Printf.bprintf b "\n") list;
    Printf.bprintf b "]"
  | VFunction _ -> Printf.bprintf b "function(...){...}"
  | VPrim s -> Printf.bprintf b "primitive(%S)" s

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
  | VString s -> [s, empty_env]
  | VList list ->
    List.map (fun v ->
      match v with
      | VString s -> s, empty_env
      | VTuple [VString s; VObject env] -> s, env
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
    if env == empty_env then VString s else
      VTuple [VString s; VObject env]
  ) list)

let plist_of_bool b = VBool b
let bool_of_plist v =
  match v with
  | VBool bool -> bool
  | VList [] -> false
  | _ -> true

let plist_of_strings strings =
  VList (List.map (fun s -> VString s) strings)
let strings_of_plist list =
  List.map (fun (s,_) -> s) (prop_list list)

let plist_of_string_option option =
  match option with
  | None -> VList []
  | Some s -> VString s

let string_option_of_plist list = match list with
  | VList [] -> None
  | _ -> Some (String.concat " " (strings_of_plist list))

let plist_of_string s = VString s
let string_of_plist list = (String.concat " " (strings_of_plist list))

let plist_of_path s = VString s
let path_of_plist list = String.concat "/" (strings_of_plist list)

let set_bool env name v = set env name (plist_of_bool v)
let get_bool env name = bool_of_plist (get env name)
let get_local_bool env name = bool_of_plist (get_local env name)

let set_strings env name v = set env name (plist_of_strings v)
let get_strings env name = strings_of_plist (get env name)
let get_local_strings env name = strings_of_plist (get_local env name)

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

    (*
let new_path_option name v =
  set_global name (plist_of_path v);
  {
    get = (fun env -> get_path env name);
    set = (fun v -> set_global name (plist_of_path v));
  }
    *)

let empty_config () = {
  config_env = empty_env;
  config_modules = ref StringMap.empty;
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
