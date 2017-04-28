(**************************************************************************)
(*                                                                        *)
(*   Typerex Tools                                                        *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU General Public License version 3 described in the file       *)
(*   LICENSE.                                                             *)
(*                                                                        *)
(**************************************************************************)

open OcpCompat
open SimpleConfig.Op (* !! and =:= *)
open AutoconfArgs

type package = {
  name : string;
  version : string option;
  opam : string option;
}

let make_package ?version ?opam name =
  { name; version; opam }

let package_option =
  let open SimpleConfig.LowLevel in
  SimpleConfig.LowLevel.define_option_class
    "package"
    (fun v ->
       match v with
       | StringValue name -> make_package name
       | List [StringValue s; StringValue version]
       | SmallList [StringValue s; StringValue version] ->
         make_package ~version s
       | Module m ->
         let name = ref None in
         let version = ref None in
         let opam = ref None in
         List.iter (fun (field, field_value) ->
             match field with
             | "name" -> name := Some (value_to_string field_value)
             | "version" -> version := Some (value_to_string field_value)
             | "opam" -> opam := Some (value_to_string field_value)
             | _ ->
               Printf.kprintf failwith "Unknown file %S in package" field
           ) m;
         begin match !name with
           | None -> failwith "Missing field 'name' in package"
           | Some name ->
             let version = !version in
             let opam = !opam in
             make_package ?version ?opam name
         end
       | _ -> failwith "Wrong package format"
    )
    (function { name; version; opam } ->
    match version, opam with
    | None, None -> StringValue name
    | Some version, None -> SmallList [StringValue name; StringValue version]
    | _ ->
      let fields = [ "name", StringValue name ] in
      let fields = match version with
          None -> fields
        | Some version -> ("version", StringValue version) :: fields in
      let fields = match opam with
          None -> fields
        | Some opam -> ("opam", StringValue opam) :: fields in
      Module fields
    )


let config_file = FileGen.of_string  "ocp-autoconf.config"
let config = SimpleConfig.create_config_file config_file

let project_name = SimpleConfig.create_option config
    [ "project_name" ]
    [ "Project Name" ] SimpleConfig.string_option
    (Filename.basename AutoconfCommon.curdir)

let project_version = SimpleConfig.create_option config
    [ "project_version" ]
    [ "Project Version" ] SimpleConfig.string_option
    "1.0"

let manage_files = SimpleConfig.create_option config
    [ "manage_files" ]
    [
"Files managed by ocp-autoconf in this project.";
"Note that 'autoconf' here means the *directory* autoconf, i.e. all files";
"in that directory.";
    ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    [ "autoconf" ]

let project_copyright = SimpleConfig.create_option config
    [ "project_copyright" ]
    [ "Project Copyright" ]
    SimpleConfig.string_option
    ""

let ocaml_minimal_version = SimpleConfig.create_option config
    [ "ocaml_minimal_version" ]
    [ "Minimal version of OCaml" ] SimpleConfig.string_option
    "3.12.0"

let ocaml_unsupported_version = SimpleConfig.create_option config
    [ "ocaml_unsupported_version" ]
    [ "Version of OCaml not yet supported" ] SimpleConfig.string_option
    ""

let need_packages = SimpleConfig.create_option config
    [ "need_packages" ]
    [ "Packages (ocamlfind) needed by the project.";
      "They can be specified as a list with items of the forms:";
      " * \"findlib\"";
      " * (\"findlib\", \"version\")";
      " * { name=\"findlib\" version=\"version\" opam=\"package\" }";
      "The later form can be used to specify a different opam package name.";
    ]
    (SimpleConfig.list_option package_option)
    []

let need_tools = SimpleConfig.create_option config
    [ "need_tools" ]
    [ "Tools needed by the project. Tested by ./configure." ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    [ "ocp-build" ]

let optional_packages = SimpleConfig.create_option config
    [ "optional_packages" ]
    [ "ocamlfind packages that could be used by the project" ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    []

let need_modules = SimpleConfig.create_option config
    [ "need_modules" ]
    [ "Modules needed by the project" ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    []

let extra_config_files = SimpleConfig.create_option config
    [ "extra_config_files" ]
    [ "Extra files to be substituted.";
      "Their paths should be related to the autoconf/ subdirectory."
    ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    []

let extra_m4_files = SimpleConfig.create_option config
    [ "extra_m4_files" ]
    [ "Extra m4 files to be added.";
      "They will be copied in autoconf/m4/." ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    []

let extra_config_vars = SimpleConfig.create_option config
    [ "extra_config_vars" ]
    [ "Extra variables to be substituted.";
      "These variables will appear directly in autoconf/Makefile.config,";
      "and as conf_xxx variables in autoconf/config.ocpgen, where xxx is";
      "their lowercase translation.";
    ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    []

let extra_bool_vars = SimpleConfig.create_option config
    [ "extra_bool_vars" ]
    [ "Extra variables to be substituted as boolean. Same as";
      "extra_config_vars, but they will appear as booleans in";
      "autoconf/config.ocpgen";

    ]
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

let default_opam_fields =    [
     "opam-version";
     "maintainer";
     "authors";
     "homepage";
     "maintainer";
     "dev-repo";
     "bug-reports";
     "build";
     "install";
     "remove";
     "depends";
     "available";
    ]

let opam_fields = SimpleConfig.create_option config
    [ "opam_fields" ]
    [ "Fields of the 'opam' file to generate.";
      "(other ones should come from the 'opam.trailer' file)." ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    default_opam_fields

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

let github_project = SimpleConfig.create_option config
    [ "github_project" ]
    [ "Name of the project on Github (Organization/Project).";
      "Other fields can be inferred from this if left empty" ]
    SimpleConfig.string_option
    ""

let dev_repo = SimpleConfig.create_option config
    [ "dev_repo" ]
    [ "URL of public development repository.";
      "If github_project is specified, the value is automatically inferred."
    ]
    SimpleConfig.string_option
    ""

let download_url_prefix = SimpleConfig.create_option config
    [ "download_url_prefix" ]
    [ "Prefix of the download URL.";
      "The download URL should be:";
      "   ${download_url_prefix}${package_version}.tar.gz.";
      "If github_project is specified, the value is automatically inferred."
    ]
    SimpleConfig.string_option
    ""

let bug_reports = SimpleConfig.create_option config
    [ "bug_reports" ]
    [ "URL where bug reports should be issued.";
      "If github_project is specified, the value is automatically inferred."
    ]
    SimpleConfig.string_option
    ""

let opam_configure_line = SimpleConfig.create_option config
    [ "opam_configure_line" ]
    [ "Line to appear in opam build instructions" ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    [
      "./configure";
      "--prefix"; "%{prefix}%";
      "--with-ocamldir=%{prefix}%/lib";
      "--with-metadir=%{prefix}%/lib";
    ]

let opam_remove_commands = SimpleConfig.create_option config
    [ "opam_remove_commands" ]
    [ "Commands to call on OPAM remove" ]
    (SimpleConfig.list_option (SimpleConfig.list_option
                                 SimpleConfig.string_option))
    []

let install_packages = SimpleConfig.create_option config
    [ "install_packages" ]
    [ "ocp-build packages to install and uninstall." ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    []

let format_version = SimpleConfig.create_option config
    [ "format_version" ]
    [ "Version of the format of this file" ]
    SimpleConfig.int_option
    0

let travis_versions = SimpleConfig.create_option config
    [ "travis_versions" ]
    [ "Versions of OCaml to build on Travis." ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    [
      "system";
      "3.12.1";
      "4.01.0";
      "4.02.3";
      "4.03.0";
      "4.04.0";
    ]

(* If the file does not exist, here is the version it should start with *)
let initial_format_version = 1

(* This is the current version *)
let current_format_version = 5

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

  if !!format_version < 2 then begin
    format_version =:= 2;

    StringMap.iter (fun file _ ->
        if not (Sys.file_exists file) then begin
          manage_files =:= file :: !!manage_files
        end
      ) !AutoconfCommon.makers;

  end;

  if !!format_version < 3 then begin
    format_version =:= 3;
  end;

  if !!format_version < 4 then begin
    format_version =:= 4;

    if !!opam_fields = [
        "opam-version";
        "build";
        "install";
        "remove";
        "available";
      ] then
      opam_fields =:= default_opam_fields;
  end;

  if !!format_version < 5 then begin
    format_version =:= 5;

    List.iter (fun (old_name, new_name) ->
      if List.mem old_name !!manage_files then
        manage_files =:=
        new_name :: (List.filter (fun s -> s <> old_name) !!manage_files)
    )
      [
        ".travis-install.sh", "autoconf/travis-install.sh";
        ".travis-ci.sh", "autoconf/travis-ci.sh";
      ]
  end;

  assert (!!format_version = current_format_version);

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

let save () =

  update_options ();
  global_to_project ();

  Printf.eprintf "Saving template file %S\n%!"
    (FileGen.to_string config_file);
  SimpleConfig.save_with_help config;
  ()

let load () =
  begin
    try
      Printf.eprintf "Loading project: %s ...%!"
        (FileGen.to_string config_file);
      SimpleConfig.load config;
      Printf.eprintf "ok\n%!";
    with
    | SimpleConfig.LoadError (_, error) as exn ->
      Printf.eprintf "failed\n%!";
      begin
        match error with
        | SimpleConfig.FileDoesNotExist ->
          Printf.eprintf "Error: %S does not exist.\n%!"
            (FileGen.to_string config_file);
          if !arg_save_template then begin
            format_version =:= initial_format_version;
            save ()
          end else begin
            Printf.eprintf "Use option --save-template to create an example.\n%!";
          end;
          exit 2
        | _ -> raise exn
      end;
    | exn ->
      Printf.eprintf "failed\n%!";
      raise exn
  end;

  if !arg_save_template ||
     !!format_version < current_format_version then save ()
