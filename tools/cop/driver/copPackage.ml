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

open BuildEngineTypes
open CopTypes

let rules_of_package b _sw pk =
  let _p = BuildEngineContext.new_package b pk.pk_name in

  (*  let build_dir = BuildEngineContext.find_directory b b.build_dir in *)
  (*  let switch_dir = BuildEngineContext *)

  assert false
