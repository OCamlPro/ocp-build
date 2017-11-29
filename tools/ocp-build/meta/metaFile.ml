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

(* let verbose = OcpDebug.verbose_function ["B"; "MetaFile"] *)

open OcpCompat

open MetaTypes

       (*
let key_of_preds preds =
  let preds = List.map (fun (s, bool) ->
    if bool then s else "-" ^ s) preds in
  String.concat ", " preds
        *)

open OcpString

let split_simplify s =
  let bs = Bytes.of_string s in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | ','
    | '\t'
    | '\n'
      -> bs.[i] <- ' '
    | _ -> ()
  done;
  let s = Bytes.to_string bs in
  OcpString.split_simplify s ' '

let split_list list =
  List.flatten (List.map split_simplify list)

(* This is the kind of things that we are supposed to support, as
ocamlfind does:

The variable "ppx" is a command that is added to the compiler
invocation via the -ppx option (available since OCaml-4.01). If the
command is relative to the current directory (e.g. ./cmd), the command
is expected in the package directory. The special forms as defined for
"archive" are also available (e.g. @otherpkg/cmd). Additional
arguments can be specified on the ocamlfind command line with the
-ppxopt option or the "ppxopt" variable.

The variable "ppxopt" is a set of options that are added to the ppx
rewriter invocation. The contents of the variable consists of one or
several whitespace-separated parts. Every part consists of several
comma-separated subparts; the first subpart indicates the package that
contains the ppx rewriter invocation, the rest contain the options to
be appended. If the option is a path relative to the current directory
(e.g. ./foo.cma), the path is expanded relative to the package
directory. The special forms as defined for "archive" are also
available (e.g. @otherpkg/foo.cma).

The following examples with ppx_driver: the ppx_driver tag is used to
discriminate between two cases:
* When using ppx_driver, the package should only add the library to
  the command line
* When not using ppx_driver, the package should trigger the call of './ppx'
  instead of using the library

version = "42.0"
description = "Blah blah"
# The following line must list the ppx dependencies of the rewriter
# itself, such as ppx_core, ppx_type_conv, ...
requires = "ppx_driver <other requiremement for rewriter itself>"
# The following line is optional, here you can list normal libraries
# that you are using inside the ppx rewriter. For instance if you are
# using the "str" library in the rewriter itself, put it here
requires(ppx_driver) = "str ..."
# The following line is for runtime dependencies, this allow users to
# just put ppx_foo as dependency and get both the rewriter and the
# runtime dependencies
requires(-ppx_driver)        += "<runtime dependencies>"
# The following line is optional, it is currently only used by
# toplevel_expect_test, which is a toplevel where the rewriting
# happens in the same process as the toplevel itself. This is useful
# for defining transformations and testing them immediately after
requires(ppx_driver,toploop) += "<runtime dependencies>"
# The next 5 lines are classic. The only difference with a normal
# library is that the archive is linked only if the "ppx_driver"
# predicate is present. This is to avoid linked them in the final
# executable itself
archive(ppx_driver, byte  ) = "ppx_expect.cma"
archive(ppx_driver, native) = "ppx_expect.cmxa"
plugin(ppx_driver, byte  ) = "ppx_expect.cma"
plugin(ppx_driver, native) = "ppx_expect.cmxs"
exists_if = "ppx_expect.cma"
# The following line instruct ocamlfind to pass "-ppx ./ppx" to the
# OCaml compiler when none of the "ppx_driver" or "custom_ppx"
# predicates are present. This explains why we need to
# "predicate(custom_ppx)" when using ppx_driver
ppx(-ppx_driver,-custom_ppx) = "./ppx"


*)

let variable_of_meta p var_name preds =
  try
    let v = StringMap.find var_name p.p_variables in
    (* Printf.eprintf "Variable %S found\n%!" var_name; *)
    let rec iter_assigns (npreds,value) assigns =
      match assigns with
        [] -> value
      | (preconds, new_value) :: assigns ->
        let ok = List.fold_left (fun preconds (precond, is_true) ->
          preconds && StringSet.mem precond preds = is_true
        ) true preconds in
        iter_assigns
          (
            if ok && List.length preconds > npreds then
              (List.length preconds,[new_value]) else
           (npreds,value)
          )
          assigns
    in
    let rec iter_additions value additions =
      match additions with
        [] -> value
      | (preconds, new_value) :: additions ->
        let ok = List.fold_left (fun preconds (precond, is_true) ->
          preconds && StringSet.mem precond preds = is_true
        ) true preconds in
        iter_additions
          (
           if ok then value @ [new_value] else value
          )
          additions
    in
    let result = iter_assigns (-1,[]) v.var_assigns in
    let result = iter_additions result v.var_additions in
    result
  with Not_found ->
    (*
    Printf.eprintf "Variable %S NOT FOUND\n%!" var_name;
    StringMap.iter (fun v _ ->
      Printf.eprintf "Variable %S exists\n%!" v
                   ) p.p_variables;
     *)
    []


let string_of_preconds preconds =
  String.concat ","
    (List.map (fun (precond, is_true) ->
      if is_true then precond else "-"^precond
     ) preconds)

let bprintf_ops oc indent op var_name ops =
  List.iter (fun (preconds, str) ->
    match preconds with
    | [] ->
      Printf.bprintf oc "%s%s %s %S\n" indent var_name op str
    | _ ->
      Printf.bprintf oc "%s%s(%s) %s %S\n" indent var_name
        (string_of_preconds preconds) op
        str
  ) ops

let string_of_meta p =
  let b = Buffer.create 11000 in
  let rec bprintf_package b indent p =
    StringMap.iter (fun var_name { var_assigns; var_additions } ->
      bprintf_ops b indent "=" var_name var_assigns;
      bprintf_ops b indent "+=" var_name var_additions
    ) p.p_variables;
    List.iter (fun (name, sub_p) ->
      Printf.bprintf b "%spackage %S (\n" indent name;
      bprintf_package b (indent ^ "  ") sub_p;
      Printf.bprintf b "%s)\n" indent;
    ) p.p_packages
  in
  bprintf_package b "" p;
  Buffer.contents b

let file_of_meta filename p =
  let oc = open_out filename in
  output_string oc (string_of_meta p);
  close_out oc

let meta_of_file filename = MetaParser.parse_file filename

type predicates = StringSet.t
let preds_none = StringSet.empty
let preds_of_strings list =
  List.fold_left (fun set s ->
    StringSet.add s set) StringSet.empty list

let preds_byte = preds_of_strings [ "byte" ]
let preds_asm = preds_of_strings [ "native" ]

let rec directory p =
  match variable_of_meta p "directory" preds_none with
  | [] ->
     (match p.p_parent with
     | None -> []
     | Some p -> directory p)
  | dir -> dir

let exists_if p = variable_of_meta p "exists_if" preds_none
let version p = variable_of_meta p "version" preds_none
let archive p preds = split_list (variable_of_meta p "archive" preds)
let requires p preds = split_list (variable_of_meta p "requires" preds)

let create = MetaParser.create

let set_var var p value =
  let v = MetaParser.get_variable p var in
  v.var_assigns <- ([], value) :: v.var_assigns

let set_directory = set_var "directory"
let set_version = set_var "version"
let set_exists_if = set_var "exists_if"
let set_description = set_var "description"

let add_var var p predicates value =
  let v = MetaParser.get_variable p var in
  let found = ref false in
  let assigns = List.map (fun (preds, previous_value) ->
    if preds = predicates && not !found then begin
      found := true;
      (preds, previous_value ^ " " ^ value)
    end
    else
      (preds, previous_value)
    ) v.var_assigns in
  v.var_assigns <- (if !found then
    assigns
  else
    (predicates, value) :: v.var_assigns
  )

let precs_byte = [ "byte", true ]
let precs_asm = [ "native", true ]

let add_archive = add_var "archive"
let add_plugin = add_var "plugin"
let add_requires = add_var "requires"

(* How -syntax works in ocamlfind ?

If [-syntax SYNTAX] is specified, then packages are interpreted
with the additional predicates "syntax" and SYNTAX. The additional
variable "preprocessor" is also computed (with the additional predicate
"preprocessor") to give an option "-pp" to ocaml.

*)
