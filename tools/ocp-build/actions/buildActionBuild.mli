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

val arg_list : (string * Arg.spec * string) list
val subcommand : BuildArgs.subcommand
val old_subcommand : BuildArgs.subcommand

val make_doc_targets : bool ref
val make_test_targets : bool ref
val make_build_targets : bool ref


val do_build :
  unit ->
  BuildActions.project_info *
    BuildTypes.builder_context * (module BuildTypes.Package) list *
  (module BuildTypes.Package) StringCompat.StringMap.t

(* val do_read_env : BuildActions.project_info -> BuildOCPInterp.state *)


val get_ncores : BuildOptions.config_input -> int
(* val finally_do : (unit -> unit) list ref  *)
