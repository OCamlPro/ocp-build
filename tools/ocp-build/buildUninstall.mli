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

type state

val init :
  string option -> (* destdir *)
  string list -> (* dirs to scan *)
  state

val uninstall : state ->  string -> unit

(* cleanup directories after removing files in them *)
val finish : state -> unit

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

val is_installed : state -> string -> bool
val list_installed : state -> package_uninstaller list
