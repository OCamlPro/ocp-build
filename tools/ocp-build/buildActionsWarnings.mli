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

type warning =
[
| BuildOCP.warning
| BuildOCamlConfig.warning
| BuildOCamlSyntaxes.warning
]

type set = warning BuildWarnings.set

val set_default_is_always : unit -> unit

(* [print_env_warnings set] prints warnings from [set],
   using [filename] as a reminder of former warnings, and update
   [filename] consequently. [kind] is a simple string to characterize
   these warnings, typically "env" or "project" *)
val print_env_warnings : set -> unit
val print_pj_warnings : set -> unit
