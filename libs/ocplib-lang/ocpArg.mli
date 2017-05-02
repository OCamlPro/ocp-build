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

type arg
type command
type runner

val arg :
  string ->
  Arg.spec ->
  ?readme:(unit -> string) -> (* readme *)
  string ->
  arg

(* a simple command *)
val basic :
  arg list ->
  (string -> unit) -> (* anon args *)
  ?readme: (unit -> string) -> (* readme *)
  string -> (* usage *)
  (unit -> unit) -> (* action *)
  command

(* a command with sub-commands *)
val group :
  arg list ->
  (string * command) list -> (* sub-commands *)
  ?readme: (unit -> string) -> (* readme *)
  string -> (* usage *)
  (unit -> unit) -> (* action *)
  command

val runner : ?version:string -> ?build:string -> unit -> runner


(* Actually parsing the arguments *)
val run : command -> runner -> unit


(* Dynamic extension of command line *)
val add_commands : command -> (string * command) list -> unit
val add_args : command -> arg list -> unit

(* From the actions *)
exception Usage of int (* print help and exit with status *)
