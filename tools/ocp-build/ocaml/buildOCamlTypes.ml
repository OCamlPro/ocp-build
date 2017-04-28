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
| CMX_O
| CMXS
| CMA
| CMXA
| CMXA_A
| STUB_A
| RUN_BYTE
| RUN_ASM

type module_origin =
    ML | MLI | MLandMLI

type ocaml_package = {
  lib : BuildTypes.package_info;
  lib_opk : ocaml_description;

  mutable lib_modules :
    (BuildEngineTypes.build_directory *
       (module_origin * string) StringMap.t ref) list;
  mutable lib_internal_modules :
    (BuildEngineTypes.build_directory *
       (module_origin * string)
        StringMap.t ref) StringsMap.t;

  lib_build_targets : BuildEngineTypes.build_file list ref;
  lib_doc_targets : BuildEngineTypes.build_file list ref;
  lib_test_targets : BuildEngineTypes.build_file list ref;

  mutable lib_autolink : bool;

  mutable lib_byte_targets : (BuildEngineTypes.build_file * target_kind) list;
  mutable lib_asm_targets : (BuildEngineTypes.build_file * target_kind) list;
  mutable lib_intf_targets : (BuildEngineTypes.build_file * target_kind) list;
  mutable lib_stub_targets : (BuildEngineTypes.build_file * target_kind) list;

  mutable lib_includes : string list option;
  mutable lib_linkdeps : ocaml_package list;

  mutable lib_sources : BuildValue.TYPES.prop_list;
  mutable lib_tests : BuildValue.TYPES.prop_list;
  mutable lib_archive : string;
  mutable lib_stubarchive : string;

  mutable lib_requires :
    ocaml_package BuildOCPTypes.package_dependency list;

  mutable lib_ready : BuildEngineTypes.build_file list;
  mutable lib_meta : bool;
}

and ocaml_description = {
  opk_name : string;
  opk_package : unit BuildOCPTypes.package;
  mutable opk_options : BuildValue.TYPES.env list;
  opk_dirname : string;
  mutable opk_version : string;
  opk_kind : package_type;

  mutable opk_install : bool;
  mutable opk_installed : bool;
  mutable opk_has_byte : bool;
  mutable opk_has_asm : bool;

  mutable opk_tolink : bool;
  mutable opk_program : bool;
  mutable opk_library : bool;

  mutable opk_build : bool;
  mutable opk_test : bool;
  mutable opk_syntax : bool;

  mutable opk_id : int; (* initialized only when sorting *)
  mutable opk_requires :
    ocaml_description BuildOCPTypes.package_dependency list;

  mutable opk_requires_map :
    ocaml_description BuildOCPTypes.package_dependency StringMap.t;
  mutable opk_usedby_map :
    ocaml_description BuildOCPTypes.package_dependency StringMap.t;
}

exception OCamlPackage of ocaml_description
