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

(* Coding standards:
   Level 0 = no debug
   Levels 1..10 = for application levels
   Levels 11..100 = for library levels
 *)

(* [verbose_function sources] returns a function [verbose], so that
  [verbose n] returns [true] if the debug level for one of the sources
  is at least [n]. *)
val verbose_function : string list -> (int -> bool)

(* [printf fmt] will print its output to the registered output of
   [OcpDebug]. The [verbose] function should be called first to check
   that the level of debugging is OK. See also [register_output]. *)
val printf : ('a, unit, string, unit) format4 -> 'a

(* [set_verbosity_env env_var] can be used to parse the [env_var]
  environment variable and use it to set the verbosity.

   The variable should contain a comma-separated list of sources,
   where each source is one of:
   * integer : sets the default verbosity to [integer];
   * name : set the source [name] verbosity by 100;
   * name=integer : set the source [name] verbosity to [integer]

   Note that "OCP_DEBUG" is always used first, this function can be used
   to add another variable.   *)
val set_verbosity_env : string -> unit

(* [set_verbosity n] sets the default verbosity. It thus applies to
all modules, regardless of their sources. *)
val set_verbosity : int -> unit

(* [set_verbosity_parse str] sets the verbosity using a string
[str]. The string must have the same format as the environment
variable content in [set_verbosity_env]. *)
val set_verbosity_parse : string -> unit

(* [set_verbosity_source src n] sets the verbosity level to [n] for
   source [src].*)
val set_verbosity_source : string -> int -> unit

(* [register_output f] is used to redirect all debug output from
  [OcpDebug.printf]. Before the first call to [register_output], the
  output is sent to [stderr].*)
val register_output : (string -> unit) -> unit
