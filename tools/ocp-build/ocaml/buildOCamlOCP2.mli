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

(* Specific function to create an OCaml package. Used to create packages
   from META descriptions somewhere else. *)
val add_ocaml_package :
  BuildValue.TYPES.location ->
  BuildOCP.state ->
  BuildValue.TYPES.config ->
  string ->
  BuildOCPTypes.package_type ->
  BuildOCamlTypes.ocaml_description

(* Initialize 'packages' with all the packages available in the
   environment *)
val init_env: BuildOCPTypes.project -> unit

(* Call to register the plugin in BuildOCP.plugin_verifiers *)
val init : unit -> unit
