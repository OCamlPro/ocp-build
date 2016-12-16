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


let verbose = DebugVerbosity.verbose ["B"] "MetaFile"

open StringCompat

open MetaTypes

let empty () = {
  meta_version = None;
  meta_description = None;
  meta_exists_if = [];
  meta_directory = None;
  meta_preprocessor = None;
  meta_name = None;
  meta_linkopts = None;
  meta_license = None;
(*  meta_browse_interfaces = []; *)

  meta_error = StringMap.empty;
  meta_requires = StringMap.empty;
  meta_archive = StringMap.empty;

  meta_package = [];
}

let key_of_preds preds =
  let preds = List.map (fun (s, bool) ->
    if bool then s else "-" ^ s) preds in
  String.concat ", " preds

let add_requires meta preds values =
  let key = key_of_preds preds in
  try
    let var = StringMap.find key meta.meta_requires in
    var.metavar_value <- var.metavar_value @ values
  with Not_found ->
    let var = {
      metavar_key = key;
      metavar_preds = preds;
      metavar_value = values;
    } in
    meta.meta_requires <- StringMap.add key var meta.meta_requires

let add_archive meta preds values =
  let key = key_of_preds preds in
  try
    let var = StringMap.find key meta.meta_archive in
    var.metavar_value <- var.metavar_value @ values
  with Not_found ->
    let var = {
      metavar_key = key;
      metavar_preds = preds;
      metavar_value = values;
    } in
    meta.meta_archive <- StringMap.add key var meta.meta_archive

let fprintf_option_field oc indent name field =
    match field with
      None -> ()
    | Some s ->
      Printf.fprintf oc "%s%s = %S\n" indent name s

let fprintf_list_field oc indent name field =
    match field with
      [] -> ()
    |  s ->
      Printf.fprintf oc "%s%s = %S\n" indent name (String.concat ", " s)

let fprintf_entries oc indent name entries =
  StringMap.iter (fun _ var ->
    Printf.fprintf oc "%s%s%s = %S\n"
      indent name
      (if var.metavar_key = "" then "" else
          Printf.sprintf "(%s)" var.metavar_key)
      (String.concat " " var.metavar_value)
  ) entries

let create_meta_file filename meta =
  let oc = open_out filename in
  let rec fprintf_meta oc indent meta =
    fprintf_option_field oc indent "version" meta.meta_version;
    fprintf_option_field oc indent "description" meta.meta_description;
    fprintf_option_field oc indent "name" meta.meta_name;
    fprintf_option_field oc indent "directory" meta.meta_directory;
    fprintf_option_field oc indent "license" meta.meta_license;
    fprintf_option_field oc indent "preprocessor" meta.meta_preprocessor;
    fprintf_option_field oc indent "linkopts" meta.meta_linkopts;
    fprintf_entries oc indent "requires" meta.meta_requires;
    fprintf_entries oc indent "archive" meta.meta_archive;
    fprintf_list_field oc indent "exists_if" meta.meta_exists_if;
    List.iter (fun (name, meta) ->
      Printf.fprintf oc "%spackage %S (\n" indent name;
      fprintf_meta oc (indent ^ "  ") meta;
      Printf.fprintf oc "%s)\n" indent;
    ) meta.meta_package
  in
  fprintf_meta oc "" meta;
  close_out oc

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

let rec meta_of_package p =
  let meta = empty () in
  StringMap.iter (fun var_name v ->
      List.iter (function
          | [], str ->
            begin
              match var_name with
              | "version" -> meta.meta_version <- Some str
              | "description" -> meta.meta_description <- Some str
              | "exists_if" -> meta.meta_exists_if <- split_simplify str
              | "directory" -> meta.meta_directory <- Some str
              | "preprocessor" -> meta.meta_preprocessor <- Some str
              | "name" -> meta.meta_name <- Some str
              | "linkopts" -> meta.meta_linkopts <- Some str

              | "requires" ->
                add_requires meta [] (split_simplify str)
              | "archive" ->
                add_archive meta [] (split_simplify str)
              | _ ->
                if verbose 4 then
                  Printf.eprintf "MetaParser.parse_file: discarding %S\n%!"
                    var_name
            end
          | preconds, str ->
            begin
              match var_name with
              | "requires" ->
                add_requires meta (List.rev preconds)
                  (OcpString.split_simplify str ' ')
              | "archive" ->
                add_archive meta (List.rev preconds)
                  (OcpString.split_simplify str ' ')
              | _ ->
                if verbose 4 then
                  Printf.eprintf "MetaParser.parse_file: discarding %S\n%!"
                    var_name
            end) v.var_assigns;
      List.iter (function
          | [], str ->
            begin
              match var_name with
                "requires" ->
                add_requires meta [] (split_simplify str)
              | "archive" ->
                add_archive meta [] (split_simplify str)
              | _ ->
                if verbose 4 then
                  Printf.eprintf "MetaParser.parse_file: discarding %S\n%!"
                    var_name
            end
          | _preconds, _str -> ()
        ) v.var_additions;
    ) p.p_variables;
  meta.meta_package <- List.map (fun (name, new_p) ->
      let new_meta = meta_of_package new_p in
      (name, new_meta)
    ) p.p_packages;
  meta


(*

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
