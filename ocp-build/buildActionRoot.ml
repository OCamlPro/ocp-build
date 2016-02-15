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



(* open BuildBase *)
open BuildArgs
open BuildOptions

(*
open BuildBase
open Stdlib2
open SimpleConfig
open BuildOCamlConfig.TYPES
open BuildEngineTypes
open BuildOCPTypes
open BuildOCPTree
open BuildTypes
open BuildGlobals
open BuildOptions
*)

let arg_list = []
let action () =
  let root_dir = BuildOptions.project_build_dirname in

  if Sys.file_exists root_dir then begin
    if not (Sys.is_directory root_dir) then begin
      Printf.eprintf "Error: cannot create %s/ directory:\n" root_dir;
      Printf.eprintf "  %s exists, but is not a directory\n" root_dir;
      BuildMisc.clean_exit 2
    end;
    (* TODO: we should probably do some check to verify that we have
    write permission everywhere in _obuild. *)
  end else
    BuildMisc.safe_mkdir BuildOptions.project_build_dirname

let subcommand = {
  sub_name = "init";
  sub_help =  "Set the root of a project.";
  sub_arg_list = arg_list;
  sub_arg_anon = None;
  sub_arg_usage = [
    "Set the root of a project.";
  ];
  sub_action = action;
}

