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

open StringCompat
open BuildValue.Types
open BuildOCP2Tree

val fatal_error : BuildOCP2Tree.location -> ('a, unit, string, 'b) format4 -> 'a
val warning : BuildOCP2Tree.location -> ('a, unit, string, unit) format4 -> 'a

val raise_type_error :
  BuildOCP2Tree.location ->
  string -> int -> string -> BuildValue.Types.value -> 'a

module Init(S: sig

    type context

    val define_package :
      context ->
      config ->
      name:string ->
      kind:string ->
      unit

    val filesubst : (string * env list) StringSubst.M.subst

  end) : sig
  val primitives :
    (
      (location -> S.context -> config -> value list -> value) *
      string list
    ) StringMap.t ref
  val primitives_help : unit -> string list StringCompat.StringMap.t

  end
