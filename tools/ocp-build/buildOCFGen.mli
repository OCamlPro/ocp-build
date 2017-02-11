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

(* [find_installed dirname] generates a file 'ocp-installed.ocp' in
directory [dirname] with a list of already installed files, that
should not be regenerated. *)
val find_installed : string -> unit

(* [gen_from_distrib dirname] generates a list of files of the form
 'ocp-installed-*.ocp' in ~/.ocp/ corresponding to the descriptions
  of all the libraries in the current distribution. *)
val gen_from_distrib : string -> unit

(* [autogen dirname target]: generate file 'build.ocp' in [dirname]
  describing a project [target], built from all the source files inside
  the directory [dirname]. *)
val autogen : string -> string -> unit
*)

