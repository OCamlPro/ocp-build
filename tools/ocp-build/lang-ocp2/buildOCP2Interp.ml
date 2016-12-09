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

module Eval(S: sig

    type context

    val filesubst : (string * env list) StringSubst.M.subst
    val define_package :
           context -> config ->
           name:string ->
           kind:string ->
           unit

    (*    if not !continue_on_ocp_error then exit 2; *)
    val parse_error : unit -> unit
    val new_file : context -> string -> string -> unit

  end) = struct

module Primitives = BuildOCP2Prims.Init(S)
let primitives_help = Primitives.primitives_help


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

let message_loc kind loc =
  Printf.kprintf (fun s ->
      Printf.eprintf "File %s, line %d:\n"
        loc.loc_begin.Lexing.pos_fname
        loc.loc_begin.Lexing.pos_lnum;
      Printf.eprintf "%s: %s\n%!" kind s;
      exit 2)

let error loc = message_loc "Error" loc
let warning loc = message_loc "Warning" loc

exception Return of BuildValue.Types.value

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
          assign_field config exp [field, loc] v
      | _ ->
        error loc "set_field generic not implemented"
    end

  | StmtReturn s_opt ->
    let v = match s_opt with
      | None -> VObject BuildValue.empty_env
      | Some v -> eval_expression ctx config v
    in
    raise (Return v)

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
      | _ ->
        error loc "Import of not an object"
      end

  | StmtInclude (file, ifelse) ->
    let file = eval_expression ctx config file in
    let filename =
      match file with
      | VString file -> file
      | _ -> (error loc "string expected" : unit); exit 2
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
    match eval_expression ctx config cond with
    | VBool true ->
      eval_statement ctx config ifthen
    | VBool false ->
      eval_statement ctx config ifelse
    | _ -> error loc "boolean expected"

and assign_field config e fields v =
  let loc = e.exp_loc in
    match e.exp_expr with
      | ExprIdent ident ->
        let env = BuildValue.config_get config ident in
        begin
          match env with
          | VObject env ->
            BuildValue.config_set config ident
              (assign_field_rec env fields v)
          | _ -> error loc "object expected for assignment"
        end
      | ExprField (e, field) ->
        assign_field config e ( (field, loc) :: fields) v
      | _ ->
        assert false

and assign_field_rec env fields v =
    match fields with
    | [] -> assert false
    | [ field, loc ] ->
      VObject (BuildValue.set env field v)
    | (field, loc) :: fields ->
      let v = BuildValue.get_local [env] field in
      match v with
      | VObject env ->
        VObject (BuildValue.set env field
                   (assign_field_rec env fields v))
      | _ ->
        error loc "object expected for assignment"

and eval_expression ctx config exp =
  let loc = exp.exp_loc in
  match exp.exp_expr with
  | ExprValue v -> v

  | ExprIdent ident ->
    begin try
        BuildValue.config_get config ident
      with Var_not_found _ ->
        if StringMap.mem ident !Primitives.primitives then
          VPrim ident
        else
          error loc "no variable %S" ident

    end

  | ExprField (v, field) ->
    let v = eval_expression ctx config v in
    begin
      match v with
      | VObject env ->
        begin try
            BuildValue.get_local [env] field
          with Var_not_found _ ->
            (*
            if StringMap.mem field !Primitives.primitives then
              VPrim field
            else *)
              error loc "no field %S" field
        end
      | _ ->
        error loc "object expected"
    end

  | ExprCall (f, args) ->
    let f = eval_expression ctx config f in
    let args = List.map (eval_expression ctx config) args in
    begin
      match f with
      | VFunction f -> f args
      | VPrim name ->
        begin
          let (f, _) =
            try
              StringMap.find name !Primitives.primitives
            with Not_found ->
              error loc "primitive %S not available" name
          in
          f ctx config args
        end
      | _ ->
        error loc "function or primitive expected"
    end

  | ExprFunction (arg_names, body) ->
    let f arg_values =
      let config = List.fold_left2 (fun config name v ->
          BuildValue.config_set config name v
        ) config arg_names arg_values in
      try
        let (_ : config) = eval_statement ctx config body in
        VObject BuildValue.empty_env
      with Return v -> v
    in
    VFunction f

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
            (VString config.config_dirname) in
        eval_statement ctx config ast
      with e ->
        Printf.eprintf "Error while interpreting file %S:\n%!" filename;
        Printf.eprintf "\t%s\n%!" (Printexc.to_string e);
        S.parse_error ();
        config

end
