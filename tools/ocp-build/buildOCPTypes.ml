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

type package_type =
  | ProgramPackage
  | TestPackage
  | LibraryPackage
  | ObjectsPackage
  | SyntaxPackage
  | RulesPackage

type 'a package = {
  mutable package_id : int;
  package_name : string; (* basename of project *)
  mutable package_dirname : string; (* where the project files are *)
  mutable package_source_kind : string; (* meta or ocp ? *)
  mutable package_provides : string; (* TODO: what the project provides,
					default "" => same as name.
					if provides is specified, then
					the name of the object should
					be that one. TODO: it should
					be an option, since it should
					apply to modules too. *)
  mutable package_type : package_type; (* what it generates *)
  mutable package_version : string;
  mutable package_auto : string option; (* unused: TODO *)

  package_loc : int;
(* Where this package is defined : *)
  package_filename : string;
(* All the .ocp files whose content can influence this package *)
  package_filenames : (string * Digest.t option) list;


  mutable package_options : env;
  pi : 'a;
}

and pre_package = unit package
and final_package = package_info package
and final_dependency = final_package package_dependency

and package_info = {
  (* at the end of "load_project", we rename package_identifiers to be
     continuous *)
  mutable package_validated : bool;
  package_node : LinearToposort.node;

  (* list of projects, on which compilation depends *)
  mutable package_deps_map : string package_dependency StringMap.t;
  (* bool = should the project be linked (true) or just a dependency (false) *)

  mutable package_requires : final_dependency list;
  mutable package_requires_map : final_dependency IntMap.t;
  mutable package_added : bool;
}

and 'a package_dependency =
    {
      dep_project : 'a;
      mutable dep_link : bool;
      mutable dep_syntax : bool;
      mutable dep_optional : bool;
      dep_options : env;
    }

and project = {
  mutable project_disabled : final_package array;
  mutable project_incomplete : final_package array;
  mutable project_sorted : final_package array;
  mutable project_missing : (string * final_package list) list;
  mutable project_conflicts :  (final_package * final_package *  final_package) list;
}
