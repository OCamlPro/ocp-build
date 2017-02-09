(**************************************************************************)
(*                                                                        *)
(*   Typerex Libraries                                                    *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* var OCP_DEBUG_MODULES is a set of space separated module names *)

val add_submodules : string -> string list -> unit
val increase_verbosity : string -> int -> unit
val increase_verbosities : string list -> int -> unit

val verbose : string list -> string -> (int -> bool)
