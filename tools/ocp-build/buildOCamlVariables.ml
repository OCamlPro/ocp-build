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


open BuildValue.Types


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
let ocamldoc_deps = BuildValue.new_strings_option "ocamldoc_deps" []
let ocamlopt_deps = BuildValue.new_strings_option "ocamlopt_deps" []
let ocamldep_deps = BuildValue.new_strings_option "ocamldep_deps" []
let ocamllex_deps = BuildValue.new_strings_option "ocamllex_deps" []
let ocamlyacc_deps = BuildValue.new_strings_option "ocamlyacc_deps" []
let ocamlmklib_deps = BuildValue.new_strings_option "ocamlmklib_deps" []
let link_deps = BuildValue.new_strings_option "link_deps" []
let asmlink_deps = BuildValue.new_strings_option "asmlink_deps" []
let bytelink_deps = BuildValue.new_strings_option "bytelink_deps" []




let mklib_option = BuildValue.new_strings_option "mklib" ([] : string list)
let bytelink_option = BuildValue.new_strings_option "bytelink" ([] : string list)
let bytecomp_option = BuildValue.new_strings_option "bytecomp" ([] : string list)
let doc_option = BuildValue.new_strings_option "doc" ([] : string list)
let asmcomp_option = BuildValue.new_strings_option "asmcomp" ([] : string list)
let asmlink_option = BuildValue.new_strings_option "asmlink" ([] : string list)
let dep_option = BuildValue.new_strings_option "dep" ([] : string list)
let bytedebug_option = BuildValue.new_bool_option "bytedebug" false
let asmdebug_option = BuildValue.new_bool_option "asmdebug" false

let force_link_option = BuildValue.new_bool_option "force_link" false

let rule_sources_option = BuildValue.new_strings_option "rule_sources" []
let more_deps_option = BuildValue.new_strings_option "more_deps" []

let linkdeps_option = BuildValue.new_strings_option "linkdeps" []

(* dependencies before preprocessing *)
let pp_requires_option = BuildValue.new_strings_option "pp_requires" []
let comp_requires_option = BuildValue.new_strings_option "comp_requires" []
let pp_deps = BuildValue.new_strings_option "pp_deps" []

(* which preprocessor command to use *)
let pp_option = BuildValue.new_strings_option "pp" []

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

(* not implemented *)
let syntax_option = BuildValue.new_strings_option "syntax" ([] : string list)
let plugin_of_option = BuildValue.new_strings_option "plugin_of" ([] : string list)
let ppflags_option = BuildValue.new_strings_option "ppflags" ([] : string list)
let pplink_option = BuildValue.new_strings_option "pplink" ([] : string list)

let package_option = BuildValue.new_string_option "package" ""

(*
let install_interface_option = BuildValue.new_bool_option "install_cmi" true
*)
let dirname_option = BuildValue.new_strings_option "dirname" ([] : string list)
let subdir_option = BuildValue.new_strings_option "subdir" ([] : string list)
let cclib_option = BuildValue.new_strings_option "cclib" ([] : string list)


let install_option = BuildValue.new_bool_option "install" true

let generated_option = BuildValue.new_bool_option "generated" false
let installed_option = BuildValue.new_bool_option "installed" false

let packages_option = BuildValue.new_option "packages" (VList [])
let features_option = BuildValue.new_strings_option "features" ([] : string list)
