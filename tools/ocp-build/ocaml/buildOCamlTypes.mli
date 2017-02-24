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

open StringCompat

module StringsMap : Map.S with type key = string list

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
  lib_opk : ocaml_description;

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
  mutable lib_linkdeps : ocaml_package list;

  mutable lib_sources : BuildValue.TYPES.prop_list;
  mutable lib_tests : BuildValue.TYPES.prop_list;
  mutable lib_archive : string;
  mutable lib_stubarchive : string;

  mutable lib_requires :
    ocaml_package BuildOCPTypes.package_dependency list;

  mutable lib_ready : BuildEngineTypes.build_file list;
  mutable lib_installed : bool;
  mutable lib_install : bool;
  mutable lib_meta : bool;
}

and ocaml_description = {
  opk_name : string;
  opk_package : unit BuildOCPTypes.package;
  mutable opk_options : BuildValue.TYPES.env;
  opk_dirname : string;
  mutable opk_version : string;
  opk_kind : BuildTypes.package_type;

  mutable opk_id : int; (* initialized only when sorting *)
  mutable opk_deps_map : string BuildOCPTypes.package_dependency StringMap.t;
  mutable opk_requires_map :
    ocaml_description BuildOCPTypes.package_dependency IntMap.t;
  mutable opk_requires :
    ocaml_description BuildOCPTypes.package_dependency list;
}
