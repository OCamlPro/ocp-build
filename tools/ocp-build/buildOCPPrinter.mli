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

val string_of_project : BuildOCPTypes.project -> string
val string_of_package :
  (OcpCompat.Buffer.t -> string -> 'a -> unit) ->
  'a BuildOCPTypes.package -> string
val eprint_project : string -> BuildOCPTypes.project -> unit
(*
val package_info : Buffer.t -> string -> BuildOCPTypes.package_info -> unit
*)
