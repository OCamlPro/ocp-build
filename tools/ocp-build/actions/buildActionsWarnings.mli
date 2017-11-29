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

open Ezcmd.Modules

val arg_list : Arg.arg_list

val set_default_is_always : unit -> unit

(* [print_env_warnings set] prints warnings from [set],
   using [filename] as a reminder of former warnings, and update
   [filename] consequently. [kind] is a simple string to characterize
   these warnings, typically "env" or "project" *)
val print_env_warnings : FileGen.t -> BuildWarnings.set -> unit
val print_pj_warnings : FileGen.t -> BuildWarnings.set -> unit
