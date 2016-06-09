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

let arg_force = ref false
let arg_git_add = ref false
let arg_save_template = ref false

let arg_list = Arg.align [
    "--save-template", Arg.Set arg_save_template,
    " Save a template if configuration file is not found";
    "--git-add", Arg.Set arg_git_add,
    " Call 'git add' at the end";
    "-f", Arg.Set arg_force,
    " Force overwrite of existing files";
  ]

let arg_usage =
  String.concat "\n" [
    Printf.sprintf "%s [OPTIONS]" (Filename.basename Sys.executable_name);
    "Available options:";
  ]
let arg_anon s =
  Printf.eprintf "Error: unexpected argument %S\n%!" s;
  Arg.usage arg_list arg_usage;
  exit 2
