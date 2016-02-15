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
open BuildValue
open BuildTypes

module StringsMap = Map.Make(struct
  type t = string list
  let compare = compare
end)

type force_kind =
  Force_IMPL
| Force_INTF
| Force_not


type pp = {
  mutable pp_option : string list;
  mutable pp_flags : BuildEngineTypes.command_argument list;
  mutable pp_requires : BuildEngineTypes.build_file list;
}

type mklib_kind =
    MKLIB_Unix
  | MKLIB_Msvc

type target_kind =
| CMI
| CMO
| CMX
| CMXS
| CMA
| CMXA
| CMXA_A
| C_A
| RUN_BYTE
| RUN_ASM

type module_origin =
    ML | MLI | MLandMLI

type ocaml_package = {
  lib : BuildTypes.package_info;

  lib_modules : (module_origin * string) StringMap.t ref;
  mutable lib_internal_modules :
    (BuildEngineTypes.build_directory *
    ((module_origin * string) StringMap.t ref)) StringsMap.t;
  mutable lib_byte_targets : (BuildEngineTypes.build_file * target_kind) list;
  lib_build_targets : BuildEngineTypes.build_file list ref;
  lib_doc_targets : BuildEngineTypes.build_file list ref;
  lib_test_targets : BuildEngineTypes.build_file list ref;
  mutable lib_cmo_objects : BuildEngineTypes.build_file list;
  mutable lib_bytecomp_deps : BuildEngineTypes.build_file list;
  mutable lib_bytelink_deps : BuildEngineTypes.build_file list;
  mutable lib_asm_targets : (BuildEngineTypes.build_file * target_kind) list;
  mutable lib_asm_cmx_objects : BuildEngineTypes.build_file list; (* .cmx *)
  mutable lib_asm_cmxo_objects : BuildEngineTypes.build_file list; (* .o *)
  mutable lib_asmcomp_deps : BuildEngineTypes.build_file list;
  mutable lib_asmlink_deps : BuildEngineTypes.build_file list;
  mutable lib_clink_deps : BuildEngineTypes.build_file list;
  mutable lib_dep_deps : BuildEngineTypes.build_file IntMap.t;

  mutable lib_includes : string list option;
  mutable lib_linkdeps : package_info list;

  mutable lib_sources : BuildValue.Types.prop_list;
  mutable lib_tests : BuildValue.Types.prop_list;
  mutable lib_archive : string;
  mutable lib_stubarchive : string;
}
