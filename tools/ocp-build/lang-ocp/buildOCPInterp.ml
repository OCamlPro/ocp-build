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

module Eval(S: sig

    type context

    val filesubst : (string * env list) BuildSubst.t
    val define_package :
      location ->
           context -> config ->
           name:string ->
           kind:string ->
           unit

    (*    if not !continue_on_ocp_error then exit 2; *)
    val parse_error : unit -> unit
    val new_file : context -> string -> string -> unit

  end) = struct


(* In version 1, [ a [ b c ] d ] is equivalent to [a b c d]. [vlist_concat]
   merges inner lists into the current list. *)
let vlist_concat list =
  VList (List.concat (List.map (fun v ->
    match v with
      VList list -> list
    | _ -> [v]
  ) list))

type prim = env list -> env -> plist

let meta_options = [
  "o",     [ "dep"; "bytecomp"; "bytelink"; "asmcomp"; "asmlink" ];
  "oc",    [ "bytecomp"; "bytelink"; "asmcomp"; "asmlink" ];
  "byte",  [  "bytecomp"; "bytelink"; ];
  "asm",   [  "asmcomp"; "asmlink"; ];
  "comp",  [  "bytecomp"; "asmcomp"; ];
  "link",  [  "bytelink"; "asmlink"; ];
  "debugflag", [ "debugflag"; "asmdebug"; "bytedebug"; ];
]

let configs = ref StringMap.empty

let define_config config config_name options =
  configs := StringMap.add config_name options !configs;
  config

let find_config config config_name =
  try
    StringMap.find config_name !configs
  with Not_found ->
    failwith (Printf.sprintf "Error: configuration %S not found\n" config_name)

let read_config_file filename =
  try
    let content = FileString.string_of_file filename in
    let digest = Digest.string content in
    Some (content, digest)
  with
  | _e ->
    Printf.eprintf "Error: file %S does not exist.\n%!" filename;
    None


module Primitives = BuildOCPPrims.Init(S)

let primitives_help = Primitives.primitives_help
let primitives = Primitives.primitives

let rec translate_toplevel_statements ctx config list =
  match list with
    [] -> config
  | stmt :: list ->
    let config = translate_toplevel_statement ctx config stmt in
    translate_toplevel_statements ctx config list

and translate_toplevel_statement ctx config stmt =
  match stmt with
  | StmtDefineConfig (config_name, options) ->
    let config_name = translate_string_expression ctx config [config.config_env] config_name in
    define_config config config_name options
  (*  (fun old_options -> translate_options old_options options); *)
  | StmtDefinePackage (package_type, library_name, simple_statements) ->
    let library_name = translate_string_expression ctx config [config.config_env] library_name in
    begin
      try
        let config = translate_statements ctx config simple_statements in
        S.define_package (BuildValue.noloc config.config_filename) ctx config
          ~name:library_name
          ~kind:package_type
      with e ->
        Printf.eprintf "Error while interpreting package %S:\n%!" library_name;
        raise e
    end;
    config
  | StmtBlock statements ->
    ignore (translate_toplevel_statements ctx config statements : config);
    config
  | StmtIfThenElse (cond, ifthen, ifelse) -> begin
      if translate_condition ctx config [config.config_env] cond then
        translate_toplevel_statements ctx config ifthen
      else
        match ifelse with
          None -> config
        | Some ifelse ->
          translate_toplevel_statements ctx config ifelse
    end
  | StmtInclude (filename, ifthen, ifelse) ->
    let filename = translate_string_expression ctx config [config.config_env] filename in
    if Filename.check_suffix filename ".ocp" then begin
      Printf.eprintf "Warning, file %S, 'include %S', file argument should not\n"
        config.config_filename filename;
      Printf.eprintf "  have a .ocp extension, as it will be loaded independantly\n%!";
    end;
    let filename = BuildSubst.subst_global filename in
    let filename = if Filename.is_relative filename then
        Filename.concat config.config_dirname filename
      else filename
    in
    let (ast, digest) =
      match read_config_file filename with
      None -> None, None
      | Some (content, digest) ->
        S.new_file ctx filename digest;
        Some (BuildOCPParse.read_ocamlconf filename content),
        Some digest
    in
    let old_filename = config.config_filename in
    let config = { config with
                   config_filenames = (filename, digest) :: config.config_filenames;
                 }
    in
    begin
      match ast, ifelse with
      | Some ast, _ ->
        let config = translate_toplevel_statements ctx { config with config_filename = filename } ast in
        translate_toplevel_statements ctx { config with config_filename = old_filename } ifthen
      | None, None -> config
      | None, Some ifelse ->
        translate_toplevel_statements ctx config ifelse
    end

  | _ -> translate_simple_statement ctx config stmt

and translate_statements ctx config list =
  match list with
    [] -> config
  | stmt :: list ->
    let config = translate_statement ctx config stmt in
    translate_statements ctx config list

and translate_statement ctx config stmt =
  match stmt with
  | StmtIfThenElse (cond, ifthen, ifelse) -> begin
      if translate_condition ctx config [config.config_env] cond then
        translate_statements ctx config ifthen
      else
        match ifelse with
          None -> config
        | Some ifelse ->
          translate_statements ctx config ifelse
    end
  | _ -> translate_simple_statement ctx config stmt

and translate_simple_statement ctx config stmt =
  match stmt with
  | StmtOption option ->
    { config with config_env =
                    translate_option ctx config
                      [] config.config_env option }
  (*    | StmtSyntax (syntax_name, camlpN, extensions) -> config *)
  | StmtIfThenElse _
  | StmtBlock _
  | StmtInclude _
  | StmtDefinePackage _
  | StmtDefineConfig _ -> assert false


and translate_condition ctx config envs cond =
  match cond with
  | IsEqual (exp1, exp2) ->
    let exp1 = translate_expression ctx config envs exp1 in
    let exp2 = translate_expression ctx config envs exp2 in
    exp1 = exp2 ||
    begin match exp1, exp2 with
    | VString (s1,_), VList [VString (s2,_)]
    | VList [VString (s1,_)], VString (s2,_) -> s1 = s2
    | _ -> false
    end

  | IsNonFalse exp ->
    let exp = try
      translate_expression ctx config envs exp
    with _ -> VBool false
    in
    BuildValue.bool_of_plist exp

  | Greater (e1,e2) ->
    let e1 = translate_expression ctx config envs e1 in
    let e2 = translate_expression ctx config envs e2 in
    BuildValue.compare_values e1 e2 = 1
  | GreaterEqual (e1,e2) ->
    let e1 = translate_expression ctx config envs e1 in
    let e2 = translate_expression ctx config envs e2 in
    BuildValue.compare_values e1 e2 >= 0

  | NotCondition cond -> not (translate_condition ctx config envs cond)
  | AndConditions (cond1, cond2) ->
    (translate_condition ctx config envs cond1)
    && (translate_condition ctx config envs cond2)
  | OrConditions (cond1, cond2) ->
    (translate_condition ctx config envs cond1)
    || (translate_condition ctx config envs cond2)

and translate_options ctx config envs env list =
  match list with
    [] -> env
  | option :: list ->
    let env = translate_option ctx config envs env option in
    translate_options ctx config envs env list

and translate_option ctx config envs env op =
  match op with
  | OptionConfigUse config_name ->
    let config_name = translate_string_expression ctx config (env :: envs) config_name in
    translate_options ctx config envs env (find_config config config_name)

  | OptionVariableSet (name, exp) ->

    (* TODO: global options *)
    let (exp : value) = translate_expression ctx config (env :: envs) exp in
    let vars = try
      List.assoc name meta_options
    with Not_found -> [ name ]
    in
    List.fold_left (fun env name -> BuildValue.set env name exp) env vars

  | OptionVariableAppend (name, exp) ->
    (* TODO: global options *)

    let exp2 = translate_expression ctx config (env :: envs) exp in

    let vars = try
      List.assoc name meta_options
    with Not_found -> [ name ]
    in
    List.fold_left (fun env name ->
      let exp1 = try BuildValue.get (env ::envs) name
      with Var_not_found _ ->
        failwith (Printf.sprintf "Variable %S is undefined (in +=)\n%!" name)
      in
      BuildValue.set env name (vlist_concat [exp1; exp2])
    ) env vars

  | OptionIfThenElse (cond, ifthen, ifelse) ->
    begin
      if translate_condition ctx config (env :: envs) cond then
        translate_option ctx config envs env ifthen
      else
        match ifelse with
          None -> env
        | Some ifelse ->
          translate_option ctx config envs env ifelse
    end
  | OptionBlock list -> translate_options ctx config envs env list

and translate_string_expression ctx config envs exp =
  match translate_expression ctx config envs exp with
    VString (s,_) |
    VList [VString (s,_)] |
    VList [VTuple [VString (s,_); _]] -> s
  | _ -> failwith "Single string expected"

and translate_expression ctx config envs exp =
(*  Printf.eprintf "translate_expression\n%!"; *)
  match exp with

  | ExprBool bool -> VBool bool
  | ExprString s -> VString (s, StringRaw)

  | ExprPrimitive (s, args) ->
    let (f, _) = try StringMap.find s !primitives with
        Not_found ->
        failwith (Printf.sprintf "Could not find primitive %S\n%!" s)
    in
    f envs (translate_options ctx config envs BuildValue.empty_env args)

  | ExprVariable name ->
    let exp = try BuildValue.get envs name
    with Var_not_found _ ->
      failwith (Printf.sprintf "Variable %S is undefined\n%!" name)
    in
    exp

  | ExprList list ->
    vlist_concat (List.map (translate_expression ctx config envs) list)

  | ExprApply (exp, args) ->
    let exp = translate_expression ctx config envs exp in
    match exp with
    | VTuple [s; VObject env] ->
      VTuple [s; VObject (translate_options ctx config envs env args)]
    | VList list ->
      VList (List.map (fun exp ->
        match exp with
        | VTuple [s; VObject env] ->
          VTuple [s; VObject (translate_options ctx config envs env args)]
        | _ ->
          VTuple [exp; VObject (translate_options ctx config envs BuildValue.empty_env args)]
      ) list)
    | _ -> VTuple [exp; VObject  (translate_options ctx config envs BuildValue.empty_env args)]

let read_ocamlconf filename =
  let (filename, ast, digest) =
    match read_config_file filename with
      None -> filename, None, None
    | Some (content, digest) ->
      filename,
      (try
         Some (BuildOCPParse.read_ocamlconf filename content)
       with BuildOCPParse.ParseError ->
         S.parse_error ();
         None),
      Some digest
  in
  fun ctx config ->
    begin match digest with
      | None -> ()
      | Some digest ->
        S.new_file ctx filename digest;
    end;
    let config = { config with
                   config_dirname = Filename.dirname filename;
                   config_filename = filename;
                   config_filenames = (filename, digest) :: config.config_filenames;
                 }
    in
    match ast with
    | None -> config
    | Some ast ->
      try
        translate_toplevel_statements ctx config
          (StmtOption (OptionVariableSet("dirname", ExprString config.config_dirname)) ::  ast)
      with e ->
        Printf.eprintf "Error while interpreting file %S:\n%!" filename;
        Printf.eprintf "\t%s\n%!" (Printexc.to_string e);
        S.parse_error ();
        config

end
