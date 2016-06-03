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
open SimpleConfig.Op (* !! and =:= *)
open AutoconfArgs

let package_option =
  let open SimpleConfig.LowLevel in
  SimpleConfig.LowLevel.define_option_class
    "package"
    (fun v ->
       match v with
       | StringValue s -> s, None
       | List [StringValue s; StringValue v]
       | SmallList [StringValue s; StringValue v] -> s, Some v
       | _ -> failwith "Wrong package format"
    )
    (function
      | s, None -> StringValue s
      | s, Some v -> SmallList [StringValue s; StringValue v]
    )


let config_file = File.of_string  "ocp-autoconf.config"
let config = SimpleConfig.create_config_file config_file

let project_name = SimpleConfig.create_option config
    [ "project_name" ]
    [ "Project Name" ] SimpleConfig.string_option
    (Filename.basename AutoconfCommon.curdir)

let project_version = SimpleConfig.create_option config
    [ "project_version" ]
    [ "Project Version" ] SimpleConfig.string_option
    "1.0"

let project_copyright = SimpleConfig.create_option config
    [ "project_copyright" ]
    [ "Project Copyright" ]
    SimpleConfig.string_option
    ""

let ocaml_minimal_version = SimpleConfig.create_option config
    [ "ocaml_minimal_version" ]
    [ "Minimal version of OCaml" ] SimpleConfig.string_option
    "3.12.1"

let ocaml_unsupported_version = SimpleConfig.create_option config
    [ "ocaml_unsupported_version" ]
    [ "Version of OCaml not yet supported" ] SimpleConfig.string_option
    ""

let need_packages = SimpleConfig.create_option config
    [ "need_packages" ]
    [ "Packages needed by the project" ]
    (SimpleConfig.list_option package_option)
    []

let need_tools = SimpleConfig.create_option config
    [ "need_tools" ]
    [ "Tools needed by the project" ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    [ "ocp-build" ]

let optional_packages = SimpleConfig.create_option config
    [ "optional_packages" ]
    [ "Packages that could be used by the project" ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    []

let need_modules = SimpleConfig.create_option config
    [ "need_modules" ]
    [ "Modules needed by the project" ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    []

let extra_config_files = SimpleConfig.create_option config
    [ "extra_config_files" ]
    [ "Extra files to be substituted" ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    []

let extra_m4_files = SimpleConfig.create_option config
    [ "extra_m4_files" ]
    [ "Extra m4 files to be added" ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    []

let extra_config_vars = SimpleConfig.create_option config
    [ "extra_config_vars" ]
    [ "Extra variables to be substituted" ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    []

let extra_bool_vars = SimpleConfig.create_option config
    [ "extra_bool_vars" ]
    [ "Extra variables to be substituted as boolean" ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    []

let need_ocamllex = SimpleConfig.create_option config
    [ "need_ocamllex" ]
    [ "Does the project need ocamllex" ] SimpleConfig.bool_option
    false

let need_ocamlyacc = SimpleConfig.create_option config
    [ "need_ocamlyacc" ]
    [ "Does the project need ocamlyacc" ] SimpleConfig.bool_option
    false

let manage_opam = SimpleConfig.create_option config
    [ "manage_opam" ]
    [ "ocp-autoconf should manage the 'opam' file" ]
    SimpleConfig.bool_option
    (not (Sys.file_exists "opam"))

let opam_fields = SimpleConfig.create_option config
    [ "opam_fields" ]
    [ "Fields of the 'opam' file to generate (other ones should come from";
      "the 'opam.trailer' file)." ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    [
      "opam-version";
      "build";
      "install";
      "remove";
      "depends";
      "available";
    ]

let opam_maintainer = SimpleConfig.create_option config
    [ "opam_maintainer" ]
    [ "Maintainer of the OPAM package" ]
    SimpleConfig.string_option
    ""

let authors = SimpleConfig.create_option config
    [ "authors" ]
    [ "Authors" ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    []

let homepage = SimpleConfig.create_option config
    [ "homepage" ]
    [ "URL of project homepage" ]
    SimpleConfig.string_option
    ""

let dev_repo = SimpleConfig.create_option config
    [ "dev_repo" ]
    [ "URL of public development repository" ]
    SimpleConfig.string_option
    ""

let github_project = SimpleConfig.create_option config
    [ "github_project" ]
    [ "Name of the project on Github (Organization/Project). Other fields can be inferred from this if left empty" ]
    SimpleConfig.string_option
    ""

let download_url_prefix = SimpleConfig.create_option config
    [ "download_url_prefix" ]
    [ "Prefix of the download URL. The download URL should be";
      "${download_url_prefix}${package_version}.tar.gz" ]
    SimpleConfig.string_option
    ""

let bug_reports = SimpleConfig.create_option config
    [ "bug_reports" ]
    [ "URL where bug reports should be issued" ]
    SimpleConfig.string_option
    ""

let opam_remove_files = SimpleConfig.create_option config
    [ "opam_remove_files" ]
    [ "Files to remove on OPAM remove" ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    []

let install_packages = SimpleConfig.create_option config
    [ "install_packages" ]
    [ "ocp-build packages to install" ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    []


let format_version = SimpleConfig.create_option config
    [ "format_version" ]
    [ "Version of the format of this file" ]
    SimpleConfig.int_option
    0

let current_format_version = 1

let update_options () =

  if !!format_version < 1 then begin
    format_version =:= 1;
    let old_project_name = SimpleConfig.create_option config
        [ "project"; "name" ]
        [ "(Deprecated, use project_name and remove this option)" ]
        SimpleConfig.string_option
        ""
    in
    let old_project_version = SimpleConfig.create_option config
        [ "project"; "version" ]
        [ "(Deprecated, use project_version and remove this option)" ]
        SimpleConfig.string_option
        ""
    in
    let old_project_copyright = SimpleConfig.create_option config
        [ "project"; "copyright" ]
        [ "(Deprecated, use project_copyright and remove this option)" ]
        SimpleConfig.string_option
        ""
    in

    if !!old_project_name <> "" then begin
      Printf.eprintf "Warning: you should remove deprecated 'project.name'\n%!";
      project_name =:= !!old_project_name;
      old_project_name =:= "";
      arg_save_template := true;
    end;
    if !!old_project_version <> "" then begin
      Printf.eprintf "Warning: you should remove deprecated 'project.version'\n%!";

      project_version =:= !!old_project_version;
      old_project_version =:= "";
      arg_save_template := true;
    end;

    if !!old_project_copyright <> "" then begin
      Printf.eprintf "Warning: you should remove deprecated 'project.copyright'\n%!";
      project_copyright =:= !!old_project_copyright;
      old_project_copyright =:= "";
      arg_save_template := true;
    end;
  end;
  ()

let global_to_project () =

  (*
  begin
    match !!AutoconfGlobalConfig.default_copyright with
    | None -> ()
    | Some copyright -> project_copyright =:= copyright
  end;
*)
  ()

let save ~update =

  update_options ();
  global_to_project ();

  Printf.eprintf "Saving template file %S\n%!"
    (File.to_string config_file);
  SimpleConfig.save_with_help config;
  ()

let load () =

  begin
    try
      SimpleConfig.load config
    with
    | SimpleConfig.LoadError (_, error) as exn ->
      match error with
      | SimpleConfig.FileDoesNotExist ->
        Printf.eprintf "Error: %S does not exist.\n%!"
          (File.to_string config_file);
        if !arg_save_template then begin
          format_version =:= current_format_version;
          save ()
        end else begin
          Printf.eprintf "Use option --save-template to create an example.\n%!";
        end;
        exit 2
      | _ -> raise exn
  end;

  if !arg_save_template ||
     !!format_version < current_format_version then save ()
