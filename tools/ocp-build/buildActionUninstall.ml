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


(* ocp-build install [OPTIONS]

  Set the options of the user preference file.

*)


open BuildArgs

open BuildGlobals


let destdir = ref None
let scandirs = ref []
let arg_list = [
  "-destdir", Arg.String (fun s ->
    destdir := Some s),
  "DIR Set root of installation dir";

  "-install-lib", Arg.String (fun s ->
    scandirs := s :: !scandirs),
  "DIR Scan directory for packages to uninstall";
]

let action () =
  if !scandirs = [] then begin
  (* TODO: we should detect which ocaml is in the PATH,
     and use its OCAMLLIB as a root. However, OPAM installs
     things one level higher... Bad design. *)
    Printf.eprintf "Error: you MUST at least use `-install-lib DIR` once.\n%!";
    exit 2
  end;

  let targets = List.rev !targets_arg in

  let state = BuildUninstall.init !destdir !scandirs in

  List.iter (BuildUninstall.uninstall state) targets;
  BuildUninstall.finish state;
  ()

let subcommand = {
  sub_name = "uninstall";
  sub_help =  "Uninstall the project.";
  sub_arg_list = arg_list;
  sub_arg_anon = Some arg_anon;
  sub_arg_usage = [ "Uninstall the project."; ];
  sub_action = action;
}
