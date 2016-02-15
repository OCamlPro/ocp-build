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

open StringCompat

open BuildArgs
open BuildOptions
open SimpleConfig

open BuildOCamlConfig.TYPES
open BuildEngineTypes
open BuildOCPTypes
open BuildOCPTree
open BuildTypes
open BuildGlobals
open BuildOptions
open BuildArgs
open BuildTerm
open BuildActions


let list_ocp_prims () =
  let prims = BuildOCPInterp.primitives_help () in
  Printf.printf "Available primitives: (use -ocp-prim PRIM for details)\n";
  StringMap.iter (fun name (_, help) ->
    Printf.printf "%%%s (_) : %s\n" name
      (match help with
         s :: _ -> s
       | [] -> "(no help available)")
  ) prims;
  Printf.printf "%!"

let ocp_prim prim =
  try
    let (_, help) = StringMap.find prim (BuildOCPInterp.primitives_help()) in
    Printf.printf "%%%s (ENV) : %s\n%!"
      prim (String.concat "\n   " help)
  with Not_found ->
    Printf.eprintf "No primitive %%%s\n%!" prim;
    BuildMisc.clean_exit 2

let arg_list = [
  "-list-ocp-prims", Arg.Unit list_ocp_prims,
  " Print the list of available primitives(%prim)";

  "-ocp-prim", Arg.String ocp_prim,
   "PRIM Display help on primitive %PRIM";
]

let action () =
  ()



let subcommand = {
  sub_name = "help";
  sub_help =  "Help On ocp-build.";
  sub_arg_list = arg_list;
  sub_arg_anon = None;
  sub_arg_usage = [ "Help on ocp-build."; ];
  sub_action = action;
}
