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

val workspace_arg : string ref
val root_arg : string option ref
val local_arg : bool ref

val common_options : Ezcmd.Modules.Arg.arg_list

(* Call [CopWorkspace.lookup_root] with the arguments provided by the user *)
val lookup_root : unit ->
                  string    (* root *)
                  * string  (* workspace file *)
