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


(* is this file an implementation source ? *)
let ml_file_option = BuildValue.new_bool_option "ml" false

(* is this file an interface source ? *)
let mli_file_option = BuildValue.new_bool_option "mli" false

(* flags to use when compiling C code *)
let cflags_option = BuildValue.new_strings_option "cflags" ([] : string list)

(* same as cflags *)
let ccopt_option = BuildValue.new_strings_option "ccopt" ([] : string list)

(* should we remove implicit dependencies towards Pervasives *)
let nopervasives = BuildValue.new_bool_option "nopervasives" false

(* should we remove implicit dependencies towards Pervasives *)
let nointernaldeps = BuildValue.new_bool_option "nointernaldeps" false

(* dependencies inferred by ocamldep that should be removed (cyclic) *)
let nodeps_option = BuildValue.new_strings_option "nodeps" []

(* cmx dependencies inferred by ocamldep that should be removed (cyclic) *)
let nocmxdeps_option = BuildValue.new_strings_option "noimpldeps" []


(* these should be fully-qualified files *)
let ocamlc_deps = BuildValue.new_strings_option "ocamlc_deps" []
let ocamlc2cma_cmd = BuildValue.new_strings_option "ocamlc2cma_cmd" []
let ocamlc2byte_cmd = BuildValue.new_strings_option "ocamlc2byte_cmd" []
let ocamldoc_deps = BuildValue.new_strings_option "ocamldoc_deps" []
let ocamlopt_deps = BuildValue.new_strings_option "ocamlopt_deps" []
let ocamlopt2cmxa_cmd = BuildValue.new_strings_option "ocamlopt2cmxa_cmd" []
let ocamlopt2asm_cmd = BuildValue.new_strings_option "ocamlopt2asm_cmd" []
let ocamldep_deps = BuildValue.new_strings_option "ocamldep_deps" []
let ocamllex_deps = BuildValue.new_strings_option "ocamllex_deps" []
let ocamlyacc_deps = BuildValue.new_strings_option "ocamlyacc_deps" []
let ocamlmklib_deps = BuildValue.new_strings_option "ocamlmklib_deps" []
let link_deps = BuildValue.new_strings_option "link_deps" []
let asmlink_deps = BuildValue.new_strings_option "asmlink_deps" []
let bytelink_deps = BuildValue.new_strings_option "bytelink_deps" []

(* additional libs to link in a program, or in a .cmxs file for a library *)
let asmlink_libs = BuildValue.new_strings_option "asmlink_libs" []
let bytelink_libs = BuildValue.new_strings_option "bytelink_libs" []

let is_toplevel = BuildValue.new_bool_option "is_toplevel" false


let mklib_option = BuildValue.new_strings_option "mklib" ([] : string list)
let bytelink_option = BuildValue.new_strings_option "bytelink" ([] : string list)
let bytecomp_option = BuildValue.new_strings_option "bytecomp" ([] : string list)
let doc_option = BuildValue.new_strings_option "doc" ([] : string list)
let asmcomp_option = BuildValue.new_strings_option "asmcomp" ([] : string list)
let asmlink_option = BuildValue.new_strings_option "asmlink" ([] : string list)
let dep_option = BuildValue.new_strings_option "dep" ([] : string list)
let bytedebug_option = BuildValue.new_bool_option "bytedebug" false
let asmdebug_option = BuildValue.new_bool_option "asmdebug" false
let debug_option = BuildValue.new_bool_option "debug" false

let force_link_option = BuildValue.new_bool_option "force_link" false

let rule_sources_option = BuildValue.new_strings_option "rule_sources" []
let more_deps_option = BuildValue.new_strings_option "more_deps" []

(* a C library that should be associated with the current library.
   It should have the same basename as the library stubarchive, but
   can be in a different directory. *)
let libstubs = BuildValue.new_string_option "libstubs" ""

(* Whether we should generate a .cmxs for the current library. true by
   default. *)
let cmxs_plugin = BuildValue.new_bool_option "cmxs_plugin" true

(* The exact order in which ALL libraries should be linked in this
   executable. Libraries are specified as projects. No dependency
   should be forgotten, even transitive ones.*)
let link_order = BuildValue.new_strings_option "link_order" []

let externals_only = BuildValue.new_bool_option "externals_only" false

(* dependencies before preprocessing *)
let pp_requires_option = BuildValue.new_strings_option "pp_requires" []
let comp_requires_option = BuildValue.new_strings_option "comp_requires" []
let pp_deps = BuildValue.new_strings_option "pp_deps" []

(* which preprocessor command to use
let pp_option = BuildValue.new_strings_option "pp" []
*)

(* should files be sorted before linking ? *)
let sort_files_option = BuildValue.new_bool_option "sort" false

(* used to implement the pack syntax *)
let pack_option = BuildValue.new_strings_option "pack" ([] : string list)

(* should we compile in bytecode *)
let byte_option = BuildValue.new_bool_option "has_byte" true
(* should we compile in native code *)
let asm_option = BuildValue.new_bool_option "has_asm" true

(* do not use the provided interface file *)
let no_mli_option = BuildValue.new_bool_option "no_mli" false


(* for dependencies *)


let syntax_option = BuildValue.new_strings_option "syntax" ([] : string list)
let ppflags_option = BuildValue.new_strings_option "ppflags" ([] : string list)
let package_option = BuildValue.new_string_option "package" ""
let subdir_option = BuildValue.new_strings_option "subdir" ([] : string list)
let cclib_option = BuildValue.new_strings_option "cclib" ([] : string list)
let packages_option = BuildValue.new_option "packages" (VList [])

let config_option =
  BuildValue.new_option "config" (VObject BuildValue.empty_env)

let ocamlmod_option =
  BuildValue.new_option "OCaml" (VObject BuildValue.empty_env)
let ocamlmod_add name v =
  match BuildValue.get_global "OCaml" with
  | VObject env ->
    BuildValue.set_global "OCaml" (VObject (BuildValue.set env name v))
  | _ -> assert false

let ocaml_option =
  BuildValue.new_option "ocaml" (VObject BuildValue.empty_env)
let ocaml_add name v =
  match BuildValue.get_global "OCaml" with
  | VObject env ->
    BuildValue.set_global "OCaml" (VObject (BuildValue.set env name v))
  | _ -> assert false

let install_META = BuildValue.new_bool_option "install_META" true
let install_subdir = BuildValue.new_string_option "install_subdir" ""

  (* not implemented *)

  (*
let plugin_of_option = BuildValue.new_strings_option "plugin_of" ([] : string list)
let pplink_option = BuildValue.new_strings_option "pplink" ([] : string list)


(*
let install_interface_option = BuildValue.new_bool_option "install_cmi" true
*)
let dirname_option = BuildValue.new_strings_option "dirname" ([] : string list)
  *)
  (*


let install_option = BuildValue.new_bool_option "install" true

let generated_option = BuildValue.new_bool_option "generated" false
let installed_option = BuildValue.new_bool_option "installed" false

let features_option = BuildValue.new_strings_option "features" ([] : string list)
  *)



type 'a specializable_type = {
  type_of_value : value -> 'a;
  type_to_value : 'a -> value;
}

type 'a specializable_field = {
  field_name : string;
  field_type : 'a specializable_type;
  field_default : 'a;
}

let (strings_type : string list specializable_type) =
  { type_of_value = BuildValue.strings_of_plist;
    type_to_value = BuildValue.plist_of_strings;
  }
let (string_type : string specializable_type) =
  { type_of_value = BuildValue.string_of_plist;
    type_to_value = BuildValue.plist_of_string;
  }

let get fd special envs =
  try
    let v = BuildValue.get envs fd.field_name in
    let v = match v with
      | VObject o ->
        (try StringMap.find special o.env with
        | Not_found ->
          StringMap.find "default" o.env)
      | _ -> v
    in
    fd.field_type.type_of_value v

  with
  | Var_not_found _ (* BuildValue.get *)
  | Not_found       (* StringMap.find *)
    -> fd.field_default

(* Two specializations: "ml" and "mli" *)
let (pp_option : string list specializable_field) =
  {
    field_name = "pp";
    field_type = strings_type;
    field_default = [];
  }
