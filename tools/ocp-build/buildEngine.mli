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


exception MissingSourceWithNoBuildingRule of BuildEngineTypes.build_rule * string

(* [init targets] Initialize the build engine, by checking activating all the rules
 needed for the creation of the files [targets].
   raise MissingSourceWithNoBuildingRule (rule, filename) if a file is needed as
      a source and no rule is available for generating it.
*)
val init :
  BuildEngineTypes.build_context ->
  BuildEngineTypes.build_file list -> unit

val fatal_errors : BuildEngineTypes.build_context -> string list list
(* val errors : unit -> string list list *)

(* [parallel_loop ncores] Start the build process on [ncores] cores. *)
val parallel_loop :
  BuildEngineTypes.build_context -> int -> unit


val sanitize :
  BuildEngineTypes.build_context ->
  BuildEngineTypes.delete_orphans ->
  (string -> bool) -> (* return false on basename if topdir is orphan *)
  int
