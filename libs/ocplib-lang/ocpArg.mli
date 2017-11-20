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

(* OcpArg: simple parsing of the command line:
   * manage subcommands and arguments
   * arguments can have aliases
 *)

(**** Specification of arguments ****)

(* abstract type of arguments *)
type arg

(* additionnal information to provide: only the short documentation is
     mandatory. *)
type arg_spec =
  ?alias:string list -> (* aliases for the argument *)
  ?readme:(unit -> string) ->
  string ->
  arg

(* Equivalent to Arg.Unit *)
val unit : string -> (unit -> unit) -> arg_spec

(* Equivalent to Arg.String. The argument can be provided either as the
next argument, or with the syntax "arg=value" *)
val string : string -> (string -> unit) -> arg_spec

(**** Specification of commands ****)

(* abstract type for commands *)
type command

(* A simple command. Unknown arguments are passed to the [action] function.
If no [action] function is provided, unknown arguments are rejected.
If there are no unknown arguments, the [action] function is called with
the empty list. The "--help" argument is automatically added if absent.
 *)
val command :
  ?args: arg list -> (* list of arguments *)
  ?group:(string * command) list -> (* list of subcommands *)
  ?readme: (unit -> string) -> (* readme *)
  ?action: (string list -> unit) -> (* action *)
  string -> (* usage *)
  command

(* Actually parsing the arguments. If [version] and/or [build] are
provided, the command arguments will be extended with "--version" and
"--build-info". *)
val run :  ?version:string -> ?build:string -> command -> unit


(* Dynamic extension of command line *)
val add_commands : command -> (string * command) list -> unit
val add_args : command -> arg list -> unit

(* exceptions to be raised from the actions *)
exception InvalidArg of string (* print error message and exit with status 1 *)
exception Usage (* print help and exit with status 0 *)
