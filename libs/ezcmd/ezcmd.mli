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

(* Easy interface over Cmdliner. Very similar to the Stdlib Arg module.

  Three main functions:
  * [Ezcmd.main cmd]: simple command
  * [Ezcmd.main_with_subcommands cmds]: with sub-commands
  * [Ezcmd.Arg.parse]: similar to Arg.parse

  Usually, you will start your code with `open Ezcmd.Modules` and then
  use functions and constructors from either `Ezcmd` or `Arg`. Also
  `Arg.translate` can be used to translate from Stdlib Arg.

 *)


module Modules : sig

  type block =
    [ `S of string
    | `P of string
    | `Pre of string
    | `I of string * string
    | `Noblank
    | `Blocks of block list ]

  module Manpage = Cmdliner.Manpage

  module Arg : sig

    type info
    type env

    type spec =
      | Bool of (bool -> unit)
      | Unit of (unit -> unit)
      | Set of bool ref
      | Clear of bool ref
      | Int of (int -> unit)
      | String of (string -> unit)
      | Strings of (string list -> unit)
      | File of (string -> unit)
      | Files of (string list -> unit)

      | Anon of int * (string -> unit)
      | Anons of (string list -> unit)

    type arg_list = (string list * spec * info) list

    type command = {
        cmd_name : string;
        cmd_action : (unit -> unit);
        cmd_args : arg_list;
        cmd_man : block list;
        cmd_doc : string;
      }

    (* Partial Compatibility with Stdlib Arg module *)
    val parse :
      ?name:string ->
      ?version:string ->
      ?man: block list ->
      (string * spec * string) list ->
      (string -> unit) ->
      string ->
      unit

    val translate :
      ?docs:string ->
      (string * spec * string) list ->
      (string list * spec * info) list

    val translate_anon:
      (string -> unit) ->
      (string list * spec * info) list

  end
end


open Modules

val env :
  ?docs:string ->
  ?doc:string ->
  string ->
  Arg.env
  (** [env ~docs ~doc var] describes an environment variable
      [var]. [doc] is the man page information of the environment
      variable, defaults to ["undocumented"]. [docs] is the title of
      the man page section in which the environment variable will be
      listed, it defaults to {!Manpage.s_environment}.

      In [doc] the {{!doclang}documentation markup language} can be
      used with following variables:
      {ul
      {- [$(env)], the value of [var].}
      {- The variables mentioned in {!info}}} *)

val info :
  ?docs:string ->
  ?docv:string ->
  ?env:Arg.env ->
  string -> (* doc *)
  Arg.info
  (** [info docs docv env doc] defines information for
      an argument.
      {ul
      {- [env] defines the name of an environment variable which is
         looked up for defining the argument if it is absent from the
         command line. See {{!envlookup}environment variables} for
         details.}
      {- [doc] is the man page information of the argument.
         The {{!doclang}documentation language} can be used and
         the following variables are recognized:
         {ul
         {- ["$(docv)"] the value of [docv] (see below).}
         {- ["$(opt)"], one of the options of [names], preference
            is given to a long one.}
         {- ["$(env)"], the environment var specified by [env] (if any).}}
         {{!doc_helpers}These functions} can help with formatting argument
         values.}
      {- [docv] is for positional and non-flag optional arguments.
         It is a variable name used in the man page to stand for their value.}
      {- [docs] is the title of the man page section in which the argument
         will be listed. For optional arguments this defaults
         to {!Manpage.s_options}. For positional arguments this defaults
         to {!Manpage.s_arguments}. However a positional argument is only
         listed if it has both a [doc] and [docv] specified.}} *)

val main_with_subcommands :
  name:string ->
  ?version:string ->
  ?default:Arg.command ->
  doc:string ->
  man:block list ->
  ?topics:(string * Cmdliner.Manpage.block list) list ->
  Arg.command list -> unit

val main :
  ?version:string ->
  Arg.command ->
  unit