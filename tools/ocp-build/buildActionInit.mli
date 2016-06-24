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

val subcommand : BuildArgs.subcommand
val action : unit -> unit
val arg_list : (string * Arg.spec * string) list

val load_initial_project :
  BuildActionsWarnings.set ->
  BuildActions.project_info ->
  BuildOCPInterp.state ->
  BuildTypes.builder_context *
    (module BuildTypes.Package) StringCompat.StringMap.t

val init_env :
  unit ->
    BuildActionsWarnings.set *
    BuildActions.project_info * BuildOCPInterp.state *
    BuildOCPTypes.project

val init_project :
  BuildActionsWarnings.set ->
  BuildActions.project_info ->
  BuildOCPInterp.state ->
  BuildTypes.builder_context *
    (module BuildTypes.Package) StringCompat.StringMap.t

val chdir_to_project : BuildActions.project_info -> unit
