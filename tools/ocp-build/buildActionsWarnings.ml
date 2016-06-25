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
open BuildOCPTypes

type warning =
[
  BuildOCP.warning
| BuildOCamlConfig.warning
| BuildOCamlSyntaxes.warning
]

type set = warning BuildWarnings.set

type print_warnings =
| PrintWarningsAlways
| PrintWarningsIfChanged
| PrintWarningsNever

let warnings_version = 1

let env_warnings_kind = "env"
let pj_warnings_kind = "project"

let arg_no_warnings_string = "--no-warnings"
let arg_warnings_fmt = format_of_string "--%s-warnings"
let arg_pj_warnings_string = Printf.sprintf arg_warnings_fmt pj_warnings_kind
let arg_env_warnings_string = Printf.sprintf arg_warnings_fmt env_warnings_kind
let arg_all_warnings_string = "--all-warnings"

let warnings_filename_fmt = format_of_string "_obuild/%s_warnings.data"

let arg_print_env_warnings = ref PrintWarningsIfChanged
let arg_print_pj_warnings = ref PrintWarningsIfChanged

let arg_list = [
  arg_no_warnings_string, Arg.Unit (fun () ->
    arg_print_pj_warnings := PrintWarningsNever;
    arg_print_env_warnings := PrintWarningsNever;
  ),
  " Print no warnings at all";
  arg_all_warnings_string, Arg.Unit (fun () ->
    arg_print_env_warnings := PrintWarningsAlways;
    arg_print_pj_warnings := PrintWarningsAlways;
  ),
  " Print all warnings, even if no changed";
  arg_env_warnings_string, Arg.Unit (fun () ->
    arg_print_env_warnings := PrintWarningsAlways;
  ),
  " Print env warnings, even if no changed";
  arg_pj_warnings_string, Arg.Unit (fun () ->
    arg_print_pj_warnings := PrintWarningsAlways;
  ),
  " Print project warnings, even if no changed";

]

let print_warning (w : warning) =
  match w with
  | `MissingDirectory (dirname, name, filename) ->
    Printf.eprintf
      "Warning: directory %S for package does not exist: \
            \   Package %S in %S disabled.\n%!"
      dirname name filename
  | `MissingTool tool ->
    Printf.eprintf "Warning: Could not find OCaml %s tool.\n" tool
  | `PackageConflict (pk1, pk2, pk3) ->
    BuildOCP.print_conflict pk1 pk2 pk3
  | `BadInstalledPackage (name1, name2) ->
    Printf.eprintf
      "Warning: installed package %s depends on source package %s\n%!"
      name1 name2
  | `MissingDependency (kind, name, dep) ->
      Printf.eprintf "Warning: missing dependency, %s %S requires %S\n%!"
        kind name dep
  | `KindMismatch (kind, name, kind2, name2) ->
    Printf.eprintf
      "Warning: %s %S depends on %S, that only exists in %s\n%!"
      kind name name2 kind2
  | `SyntaxDepDeclaredAsNotSyntax (lib_name, tool_name, pk_name) ->
    Printf.eprintf "Warning: in package %S, %s_requires: dependency %S not declared as syntax" lib_name tool_name pk_name
  | `SyntaxDepNotDeclared (lib_name, tool_name, pk_name) ->
    Printf.fprintf stderr "Warning: in package %s, %s_requires dependency %S not declared\n%!"
      lib_name tool_name pk_name
  | `IncompletePackage pk ->
    Printf.eprintf "Warning: package %S disabled\n" pk.package_name
  | `MissingPackage (name, pks) ->
    Printf.eprintf "Warning: missing package %S, needed by\n" name;
    List.iter (fun pk ->
      Printf.eprintf "  * %S\n%!" pk.package_name) pks


let print_warnings arg_print_warnings warnings_kind w =
  let warnings_filename = Printf.sprintf warnings_filename_fmt warnings_kind in
  let count = BuildWarnings.count w in
  if count = 0 then begin
    (try Sys.remove warnings_filename with _ -> ());
  end else begin
    if
        match arg_print_warnings with
      | PrintWarningsNever ->
        BuildWarnings.clear w;
        Printf.eprintf
          "Warning: %d %s warnings were not printed (remove %s)\n%!"
          count warnings_kind arg_no_warnings_string;
        false

      | PrintWarningsIfChanged ->
        let old_w =
          try
            let ic = open_in_bin warnings_filename in
            let (version : int) = input_value ic in
            if version <> warnings_version then raise Exit;
            let (w : 'a BuildWarnings.set) = input_value ic in
            close_in ic;
            w
          with _ ->
            BuildWarnings.empty_set ()
        in
        let equal = BuildWarnings.equal w old_w in
        if equal then begin
          Printf.eprintf
            "Warning: %d old %s warnings were not printed (add %(%s%))\n%!"
            count warnings_kind arg_warnings_fmt warnings_kind
        end;
        not equal
      | PrintWarningsAlways -> true

        then begin
          Printf.eprintf "----- %d %s warnings -----\n" count warnings_kind;
          BuildWarnings.iter print_warning w
        end;
        let oc = open_out_bin warnings_filename in
        output_value oc warnings_version;
        output_value oc w;
        close_out oc
    end

let set_default_is_always () =
  if !arg_print_env_warnings = PrintWarningsIfChanged then
    arg_print_env_warnings := PrintWarningsAlways;
  if !arg_print_pj_warnings = PrintWarningsIfChanged then
    arg_print_pj_warnings := PrintWarningsAlways;
  ()

let print_env_warnings w =
  print_warnings !arg_print_env_warnings env_warnings_kind w

let print_pj_warnings w =
  print_warnings !arg_print_pj_warnings pj_warnings_kind  w
