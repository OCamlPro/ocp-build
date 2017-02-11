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

open StringCompat
open BuildValue.Types
open BuildOCP2Tree

module Eval(S: sig

    type context

    (* a substitution that can be applied *)
    val filesubst : (string * env list) StringSubst.M.subst


    val define_package :
      location ->
      context ->
      config ->
      name:string ->
      kind:string ->
      unit

    (* [parse_error()] is called in case of syntax error, to
       decide what to do next (raise an exception, exit or continue). *)
    val parse_error : unit -> unit

    (* [new_file ctx filename digest] is called for every file that
       is read *)
    val new_file : context -> string -> string -> unit

  end) : sig


  (* [read_ocamlconf filename] returns a function [eval] that
     can evaluate the AST on [eval ctx config]. *)
  val read_ocamlconf : string -> (S.context -> config -> config)

  (* Used to display language help on command-line *)
  val primitives_help : unit -> string list StringMap.t
 val add_primitive :
           string ->
           string list ->
           (BuildValue.Types.location ->
            S.context ->
            BuildValue.Types.config ->
            BuildValue.Types.value list -> BuildValue.Types.value) ->
           unit

end
