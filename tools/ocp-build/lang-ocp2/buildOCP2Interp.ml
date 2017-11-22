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
open BuildOCP2Prims

module Eval(S: sig

  type context

    (*    if not !continue_on_ocp_error then exit 2; *)
  val parse_error : unit -> unit
  val new_file : context -> string -> string -> unit

end) = struct

module Primitives = BuildOCP2Prims.Init(S)
let primitives_help = Primitives.primitives_help

let add_primitive = Primitives.add_primitive
let apply_fun = Primitives.apply_fun

let read_config_file filename =
  try
    let content = FileString.string_of_file filename in
    let digest = Digest.string content in
    Some (content, digest)
  with
  | _e ->
    Printf.eprintf "Error: file %S does not exist.\n%!" filename;
    None

(* Before this line is exactly the same as in BuildOCPInterp.ml *)

exception OCPReturn of BuildValue.TYPES.value

let rec eval_statement ctx config stmt =
  let loc = stmt.stmt_loc in
  match stmt.stmt_expr with
  | StmtSeq (s1, s2) ->
    let config = eval_statement ctx config s1 in
    eval_statement ctx config s2
  | StmtEmpty -> config
  | StmtAssign (lhs, expr) ->
    let v = eval_expression ctx config expr in
    begin match lhs.exp_expr with
      | ExprIdent s ->
        BuildValue.config_set config s v
      | ExprField( exp, field) ->
        let fields = eval_field ctx config field in
        assign_field ctx config exp fields v
      | _ ->
        ocp2_raise loc "not-implemented"
          (VString ("%set-field generic", StringRaw))
    end

  | StmtReturn s_opt ->
    let v = match s_opt with
      | None -> VObject BuildValue.empty_env
      | Some v -> eval_expression ctx config v
    in
    raise (OCPReturn v)

  | StmtExpr exp ->
    let (_ : value) = eval_expression ctx config exp in
    config

  | StmtBlock stmt ->
    let (_ : config) = eval_statement ctx config stmt in
    config

  | StmtImport expr ->
    begin
      match eval_expression ctx config expr with
      | VObject env ->
        StringMap.fold (fun name v config ->
            BuildValue.config_set config name v
          ) env.env config
      | v ->
        raise_type_error loc "%import" 1 "object" v
    end

  | StmtInclude (file, ifelse) ->
    let file = eval_expression ctx config file in
    let filename =
      match file with
      | VString (file,_) -> file
      | v ->
        raise_type_error loc "%include" 1 "string" v
    in
    if Filename.check_suffix filename ".ocp2" then
      warning loc "avoid extension .ocp2 for file %S" filename;
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
        Some (BuildOCP2Parse.read_ocamlconf filename content),
        Some digest
    in
    let old_filename = config.config_filename in
    let config = {
      config with
      config_filenames = (filename, digest) :: config.config_filenames;
    }
    in
    begin
      match ast with
      | Some ast ->
        let config = eval_statement ctx
            { config with config_filename = filename } ast in
        { config with config_filename = old_filename }
      | None ->
        eval_statement ctx config ifelse
    end

  | StmtIfthenelse (cond, ifthen, ifelse) ->
    begin
      match eval_expression ctx config cond with
      | VBool true ->
        eval_statement ctx config ifthen
      | VBool false ->
        eval_statement ctx config ifelse
      | v ->
        raise_type_error loc "%if" 1  "bool" v
    end

  | StmtTry (body, handlers) ->
    begin
      try
        eval_statement ctx config body
      with OCPExn (loc, name, v) as e ->
      try
        let (ident, handler) = List.assoc name handlers in
        let config =
          BuildValue.config_set config ident v
        in
        eval_statement ctx config handler
      with Not_found ->
        raise e
    end

  | StmtFor (ident, range, body) ->
    let range = eval_expression ctx config range in
    begin
      match range with
      | VList list -> for_list ctx config ident list body
      | VTuple [VInt n1; VInt n2] ->
        for_int ctx config ident n1 n2 1 body
      | VTuple [VInt n1; VInt step; VInt n2] ->
        for_int ctx config ident n1 n2 step body
      | _ -> raise_type_error loc "%for" 1 "range" range
      end

and for_list ctx config ident list body =
  match list with
  | [] -> config
  | v :: list ->
    let config = BuildValue.config_set config ident v in
    let config = eval_statement ctx config body in
    for_list ctx config ident list body

and for_int ctx config ident v1 v2 step body =
  if v1 <= v2 then
    let config = BuildValue.config_set config ident (VInt v1) in
    let config = eval_statement ctx config body in
    for_int ctx config ident (v1+step) v2 step body
  else config

and assign_field ctx config e fields v =
  let loc = e.exp_loc in
  match e.exp_expr with
  | ExprIdent ident
  | ExprValue (VString (ident,_)) ->
    let env = try
        BuildValue.config_get config ident
      with Var_not_found _ -> VObject BuildValue.empty_env
    in
    begin
      match env with
      | VObject env ->
        BuildValue.config_set config ident
          (assign_field_rec loc env fields v)
      | _ ->
        raise_type_error loc "%set-field" 1  "object" env
    end
  | ExprField (e, field) ->
    let new_fields = eval_field ctx config field in
    assign_field ctx config e ( new_fields @ fields) v
  | _ ->
    assert false

and assign_field_rec loc env fields v =
  match fields with
  | [] -> assert false
  | [ field, loc ] ->
    VObject (BuildValue.set env field v)
  | (field, loc) :: fields ->
    let v2 = try
        BuildValue.get_local [env] field
      with Var_not_found _ -> VObject BuildValue.empty_env
    in
    begin
      match v2 with
      | VObject env2 ->
        VObject (BuildValue.set env field
                   (assign_field_rec loc env2 fields v))
      | _ ->
        raise_type_error loc "%set-field-rec" (List.length fields) "object" v
    end

and eval_field ctx config exp =
  let field = eval_expression ctx config exp in
  match field with
  | VTuple list ->
    List.map (field_name_of_field exp) list
  | _ -> [field_name_of_field exp field]

and field_name_of_field exp field =
  let field_name =
  match field with
  | VString (s,_) -> s
  | VInt n -> string_of_int n
  | VBool true -> "true"
  | VBool false -> "false"
  | _ ->
    raise_type_error exp.exp_loc "%field" 1  "string" field
  in
  field_name, exp.exp_loc

and eval_expression ctx config exp =
  let loc = exp.exp_loc in
  match exp.exp_expr with
  | ExprValue v -> v

  | ExprIdent ident ->
    begin try
        BuildValue.config_get config ident
      with Var_not_found _ ->
        if StringMap.mem ident !Primitives.primitives then
          VFun (VPrim ident)
        else
          raise (OCPExn (loc, "unknown-variable", VString (ident, StringRaw)))
    end

  | ExprField (v, field) ->
    let v = eval_expression ctx config v in
    begin
      match v with
      | VObject env ->
        let fields = eval_field ctx config field in
        let rec get_fields value fields =
          match fields with
          | [] -> value
          | (field, loc) :: fields ->
              match value with
              | VObject env ->
                let value =
                  try
                    BuildValue.get_local [env] field
                  with Var_not_found _ ->
                    (*
                      if StringMap.mem field !Primitives.primitives then
                      VPrim field
                      else *)
                    raise (OCPExn (loc, "unknown-field",
                                   VString (field, StringRaw)))
                in
                get_fields value fields
              | _ ->
                raise_type_error loc "%get-field(value,field)" 1 "object" value
        in
        get_fields (VObject env) fields
      | VList list | VTuple list ->
        let field = eval_expression ctx config field in
        begin
          match field with
          | VInt n ->
            (try
               List.nth list n
             with _ ->
               ocp2_raise loc "invalid-list-access"
                 (VTuple [field;v])
            )
          | _ ->
            raise_type_error loc "%get-field(list,int)" 1 "object" v
        end

      | _ ->
        raise_type_error loc "%get-field(object|list,index)" 1 "object" v
    end

  | ExprEnv -> VObject config.config_env

  | ExprCall (f, args) ->
    let f = eval_expression ctx config f in
    let args = List.map (eval_expression ctx config) args in
    begin
      match f with
      | VFun (VFunction f) -> f loc args
      | VFun (VPrim name) ->
        begin
          let (f, _) =
            try
              StringMap.find name !Primitives.primitives
            with Not_found ->
              fatal_error loc "primitive %S not available" name
          in
          f loc ctx config args
        end
      | _ ->
        raise_type_error loc "%apply" 1 "function" f
    end

  | ExprFunction (arg_names, body) ->
    let arity = List.length arg_names in
    let f loc arg_values =
      let arg_values =
        if arity <> List.length arg_values then
          match List.rev arg_names with
            ".." :: var_arg :: strict_args ->
              let strict = List.length strict_args in
              let rec split_args strict strict_args args =
                if strict = 0 then
                  (List.rev strict_args) @ [VList args; VList args]
                else
                  match args with
                  | [] ->
                    BuildOCP2Prims.raise_bad_arity loc "function"
                      (arity-2) arg_values
                  | arg :: args ->
                    split_args (strict - 1) (arg :: strict_args) args
              in
              split_args strict [] arg_values
          | _ ->
            BuildOCP2Prims.raise_bad_arity loc "function" arity arg_values
        else arg_values
      in
      let config = List.fold_left2 (fun config name v ->
          BuildValue.config_set config name v
        ) config arg_names arg_values in
      try
        let (_ : config) = eval_statement ctx config body in
        VObject BuildValue.empty_env
      with OCPReturn v -> v
    in
    VFun (VFunction f)

  | ExprRecord nvs ->
    let env = List.fold_left (fun env (name,v) ->
        let v = eval_expression ctx config v in
        BuildValue.set env name v
      ) BuildValue.empty_env nvs in
    VObject env

  | ExprList vs ->
    VList (List.map (eval_expression ctx config) vs)

  | ExprTuple vs ->
    VTuple (List.map (eval_expression ctx config) vs)


  | ExprTry (body, handlers) ->
    begin
      try
        eval_expression ctx config body
      with OCPExn (loc, name, v) as e ->
      try
        let (ident, handler) = List.assoc name handlers in
        let config =
          BuildValue.config_set config ident v
        in
        eval_expression ctx config handler
      with Not_found ->
        raise e
    end


(* after this line is exactly the same as in BuildOCPInterp.ml *)

let read_ocamlconf filename =
  let (filename, ast, digest) =
    match read_config_file filename with
      None -> filename, None, None
    | Some (content, digest) ->
      filename,
      (try
         Some (BuildOCP2Parse.read_ocamlconf filename content)
       with BuildOCP2Parse.ParseError ->
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
    let config = {
      config with
      config_dirname = Filename.dirname filename;
      config_filename = filename;
      config_filenames = (filename, digest) :: config.config_filenames;
    }
    in
    match ast with
    | None -> config
    | Some ast ->
      try
        let config = BuildValue.config_set config "dirname"
            (VString (config.config_dirname, StringRaw)) in
        eval_statement ctx config ast
      with
      | OCPExn (loc, name, arg) ->
        Printf.eprintf "File %S, line %d:\n"
          loc.loc_begin.Lexing.pos_fname
          loc.loc_begin.Lexing.pos_lnum;
        Printf.eprintf "Error: fatal exception %S:\n%!" name;
        let b = Buffer.create 111 in
        BuildValue.bprint_value b "  " arg;
        Printf.eprintf "   with arg %s\n%!" (Buffer.contents b);
        S.parse_error ();
        config

      | e ->
        Printf.eprintf "Error while interpreting file %S:\n%!" filename;
        Printf.eprintf "\t%s\n%!" (Printexc.to_string e);
        S.parse_error ();
        config

end
