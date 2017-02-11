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

val plugin : (module BuildTypes.Plugin)

(* From the [validated_projects] table, fill the other
   tables *)
val create :
  BuildWarnings.set ->
  BuildOptions.config_input ->
  BuildOCamlConfig.TYPES.config_output ->
  BuildTypes.builder_context ->
  BuildOCPTypes.project ->
  (module BuildTypes.Package) array
