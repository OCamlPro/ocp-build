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


module TYPES : sig

  type subcmd_spec = {
    subcmd_list : (Arg.key * Arg.spec * Arg.doc) list;
    subcmd_usage : string list;
    subcmd_help : string list;
  }

  type subcmd_init = (unit -> unit)
  type subcmd_action = (string array -> unit)

end

open TYPES

module type SPEC = sig

  val subcmd_spec : subcmd_spec
  val subcmd_init : subcmd_init
  val subcmd_main : subcmd_action

end

exception Usage

val parse :
(Arg.key * Arg.spec * Arg.doc) list ->
  (string * subcmd_init * subcmd_spec * subcmd_action) list ->
  Arg.usage_msg ->
  unit
