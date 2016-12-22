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


open BuildValue.Types

let add_primitive name prim_help prim =
  let prim_name = "OCaml." ^ name in
  BuildOCamlVariables.ocamlmod_add name (VPrim prim_name);
  BuildOCP.add_primitive prim_name prim_help prim


let add_ocaml_package state config name kind =
  let (_ : unit BuildOCPTypes.package) =
    BuildOCP.define_package state config ~name ~kind
  in
  ()

let _ =
  (* For .ocp files, we can only define OCaml packages, so
     BuildOCP has a specific way to do it ! *)
  BuildOCP.add_ocaml_package := add_ocaml_package;

  add_primitive "new_package"
    [ "Add a new OCaml package" ]
    (fun loc state config args ->
      match args with
      | [ VString name; VString kind; VObject config_env ] ->
        add_ocaml_package state { config  with config_env }
          name (BuildOCP.package_type_of_string kind);
        BuildValue.unit
      |  _ ->
        BuildOCP2Prims.raise_bad_arity loc
          "OCaml.new_package(name,kind,ocaml)" 3 args
    );
  add_primitive "library"
    [ "Add a new OCaml library" ]
    (fun loc state config args ->
      match args with
      | [ VString name; VObject config_env ] ->
        add_ocaml_package state { config  with config_env }
          name BuildOCPTypes.LibraryPackage;
        BuildValue.unit
      |  _ ->
        BuildOCP2Prims.raise_bad_arity loc
          "OCaml.library(name,ocaml)" 2 args
    );
  add_primitive "program"
    [ "Add a new OCaml program" ]
    (fun loc state config args ->
      match args with
      | [ VString name; VObject config_env ] ->
        add_ocaml_package state { config  with config_env }
          name BuildOCPTypes.ProgramPackage;
        BuildValue.unit
      |  _ ->
        BuildOCP2Prims.raise_bad_arity loc
          "OCaml.library(name,ocaml)" 2 args
    )


(* This [getconf] primitive should probably be moved to BuildOCP,
as it is not specific to OCaml, isn't it ? *)
let () =
  BuildOCP.add_primitive "getconf"
    [ "getconf(name) returns the configuration associated with name" ]
    (fun loc state config args ->
      match args with
      | [VString name] ->
        begin
          try
            begin match BuildValue.config_get config "config" with
            | VObject env ->
              BuildValue.get [env] name
            | _ -> BuildValue.unit
            end
          with Var_not_found _ -> BuildValue.unit
        end
      | _ ->
        BuildOCP2Prims.raise_bad_arity loc
          "getconf(name)" 1 args
    )
