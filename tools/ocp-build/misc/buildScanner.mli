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

exception IgnoreDirectory

val ignore_file_or_directory : unit -> 'a

val scan_directory_for_suffix :
 (* directory *) string -> (* extension *) string ->
  (string -> unit) -> unit

val scan_directory_for_files :
 (* directory *) string ->
 (* extensions handlers *)
  (string -> unit) StringMap.t ->
  unit

val scan_directory_for_extensions :
 (* directory *) string ->
 (* extensions handlers *)
  (string -> unit) StringMap.t ->
  unit

val scan_directory : (string (*dirname*) ->
                      string (*basename*) ->
                      string (*fullname*) -> unit)
  -> string -> unit
