(**************************************************************************)
(*                                                                        *)
(*                              OCamlPro TypeRex                          *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*      (GNU Public Licence version 3.0).                                 *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)


 (* comma or space separated *)

val empty : unit -> MetaTypes.meta
val create_meta_file : string -> MetaTypes.meta -> unit
val add_requires : MetaTypes.meta -> (string * bool) list -> string list -> unit
val add_archive : MetaTypes.meta -> (string * bool) list -> string list -> unit
val split_simplify : string -> string list

val meta_of_package : MetaTypes.meta_package -> MetaTypes.meta
val file_of_package : string -> MetaTypes.meta_package -> unit

(* [variable_of_package p var_name preconds] returns a list of strings
   corresponding to the value of [var_name] as defined in the package
   [p] with the predicates [preconds]. The variable is initialized with
   the empty list, which is returned if the value is not set in the
   package. *)
val variable_of_package :
  MetaTypes.meta_package ->    (* META *)
  string ->                    (* wanted variable *)
  StringCompat.StringSet.t ->  (* predicates *)
  string list                  (* result *)
