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

(* This is called when the plugin is loaded, or from the main if the plugin
 is linked statically. It is called after the subcommands has been chosen,
   allowing a right choice of arguments. *)

let init subcommand_name =

  Printf.eprintf "Init OCamlPlugin for %S\n%!" subcommand_name;

  let add_args subs args =
    if List.mem subcommand_name subs then
      BuildGlobals.arg_list := !BuildGlobals.arg_list @ args
  in

  add_args ["SUBCOMMAND"; "build"]
    [
      "--print-incomplete-packages", StdlibArg.Set
        BuildOCamlPackage.print_incomplete_packages,
      " Print incomplete packages";

    ];

  ()
