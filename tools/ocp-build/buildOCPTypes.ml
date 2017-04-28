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

type package_type =
  | ProgramPackage
  | TestPackage
  | LibraryPackage
  | ObjectsPackage
  | SyntaxPackage
  | RulesPackage

type 'a package = {
  mutable package_id : int;
  package_name : string; (* basename of project *)
  mutable package_dirname : string; (* where the project files are *)
  mutable package_source_kind : string; (* meta or ocp ? *)
  mutable package_type : package_type; (* what it generates *)

  package_loc : location;
(* Where this package is defined : *)
  package_filename : string;
(* All the .ocp files whose content can influence this package *)
  package_filenames : (string * Digest.t option) list;

  (*  mutable package_options : env; *)
  mutable package_plugin : exn;

  (* disabled = Some reason; enabled = None *)
  mutable package_disabled : string option;
  mutable package_requires_list : 'a package list;
  mutable package_node : OcpToposort.node;
}

and package_info = unit
and pre_package = unit package
and final_package = package_info package
and final_dependency = final_package package_dependency

and 'a package_dependency =
    {
      dep_project : 'a;
      mutable dep_link : bool;
      mutable dep_syntax : bool;
      mutable dep_optional : bool;
      dep_options : env;
    }

and project = {
  mutable project_sorted : final_package array;
  mutable project_disabled : final_package array;
}

let package_disabled pk = pk.package_disabled != None
let package_enabled pk = pk.package_disabled == None
