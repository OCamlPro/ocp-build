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

module TYPES: sig
type package_uninstaller = {
  mutable un_nfiles : int;
  mutable un_ndirs : int;
  mutable un_version : string;
  mutable un_name : string;
  mutable un_descr : string;
  mutable un_warning : string option;
  mutable un_directory : string;
  mutable un_type : string;
  mutable un_packages : string list;
}

type raw_kind = DIR | FILE | VERSION | WARNING | TYPE | PACK

end

open TYPES

(* The list of uninstallers on the system *)
type state

(* [init ?destdir dirs] search for uninstallers in the system and returns the corresponding state *)
val init :
  string option -> (* destdir *)
  string list -> (* dirs to scan *)
  state

(* [uninstall state package_name] uninstall a package whose
   uninstaller is available in state *)
val uninstall : state ->  string -> unit

(* cleanup directories after removing files in them *)
val finish : state -> unit

val is_installed : state -> string -> bool
val list_installed : state -> package_uninstaller list




type raw_uninstaller
val create_un : unit -> raw_uninstaller
val add_un_field : raw_uninstaller -> raw_kind -> string -> unit
val save_un : string -> raw_uninstaller -> unit
