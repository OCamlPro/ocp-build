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

let (!!) = SimpleConfig.(!!)
let (=:=) = SimpleConfig.(=:=)

let config_file = File.of_string  "ocp-autoconf.config"
let config = SimpleConfig.create_config_file config_file

let arg_git_add = ref false
let arg_save_template = ref false

let arg_list = Arg.align [
    "--save-template", Arg.Set arg_save_template,
    " Save a template if configuration file is not found";
    "--git-add", Arg.Set arg_git_add,
    " Call 'git add' at the end";
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

let () =
  Arg.parse arg_list arg_anon arg_usage


let files = AutoconfFiles.files

let find_content filename =
  try
    List.assoc filename files
  with Not_found ->
    Printf.eprintf "Template for file %S not found\n%!" filename;
    exit 2

let save_file ?(override=true) filename =
  assert (OcpString.starts_with filename "skeleton/");
  let _,dst_filename = OcpString.cut_at filename '/' in
  if override || not (Sys.file_exists dst_filename) then
    let content = find_content filename in
    let dirname = Filename.dirname dst_filename in
    FileString.safe_mkdir dirname;
    FileString.write_file dst_filename content;
    Printf.eprintf "* %s saved\n%!" dst_filename;
    ()

let curdir = Sys.getcwd ()

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

let project_name = SimpleConfig.create_option config
    [ "project_name" ]
    [ "Project Name" ] SimpleConfig.string_option
    (Filename.basename curdir)

let project_version = SimpleConfig.create_option config
    [ "project_version" ]
    [ "Project Version" ] SimpleConfig.string_option
    "1.0"

let project_copyright = SimpleConfig.create_option config
    [ "project_copyright" ]
    [ "Project Copyright" ] SimpleConfig.string_option
    "Copyright 2016"

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

let command cmd =
  Printf.eprintf "Calling %s...\n%!" cmd;
  let code = Sys.command cmd in
  if code <> 0 then begin
    Printf.eprintf "Error: %S returned non-zero status (%d)\n%!" cmd code;
    exit 2
  end


let () =

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
          Printf.eprintf "Saving template file.\n%!";
          SimpleConfig.save_with_help config
        end else
          Printf.eprintf "Use option --save-template to create an example.\n%!";
        exit 2
      | _ -> raise exn
  end

let old_project_name = SimpleConfig.create_option config
    [ "project"; "name" ]
    [ "(Deprecated, use project_name and remove this option)" ]
    SimpleConfig.string_option
    ""

let old_project_version = SimpleConfig.create_option config
    [ "project"; "version" ]
    [ "(Deprecated, use project_version and remove this option)" ]
    SimpleConfig.string_option
    ""

let old_project_copyright = SimpleConfig.create_option config
    [ "project"; "copyright" ]
    [ "(Deprecated, use project_copyright and remove this option)" ]
    SimpleConfig.string_option
    ""

let extra_files = ref []

let () =

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

  if !arg_save_template then begin
    Printf.eprintf "Saving template file.\n%!";
    SimpleConfig.save_with_help config
  end;

  let need_pkgs = ref [] in
  let need_ocamlbuild = ref false in
  let need_ocamlfind = ref false in
  let need_camlp4 = ref false in

  List.iter (fun (package, version) ->
      match package with
      | "ocamlbuild" -> need_ocamlbuild := true
      | "ocamlfind" -> need_ocamlfind := true
      | "camlp4" -> need_camlp4 := true
      | _ ->
        need_pkgs := (package, version) :: !need_pkgs
    ) !!need_packages;
  if !!optional_packages <> [] then need_ocamlfind := true;

  let need_packages = List.rev !need_pkgs in

  FileString.safe_mkdir "autoconf";
  Printf.eprintf "Saving template files...\n%!";
  List.iter save_file [
    "skeleton/autoconf/m4/ax_compare_version.m4";
    "skeleton/autoconf/m4/ocaml.m4";
    "skeleton/autoconf/Makefile.rules";
    "skeleton/autoconf/.gitignore";
  ] ;
  if not (Sys.file_exists "configure") then begin
    Printf.eprintf "Warning: ./configure, creating one.\n%!";
    FileString.write_file "configure"
      (find_content "skeleton/configure");
    Unix.chmod "configure"  0o755;
  end;
  List.iter (fun filename ->
      let content = FileString.read_file filename in
      let basename = Filename.basename filename in
      let dst_filename = Filename.concat "autoconf/m4" basename in
      extra_files := dst_filename :: !extra_files;
      FileString.write_file dst_filename content
    ) !!extra_m4_files;


  List.iter (save_file ~override:false) [
    "skeleton/build.ocp";
    "skeleton/Makefile";
    "skeleton/ocp-autoconf.ac";
    "skeleton/.gitignore";
    "skeleton/LICENSE";
  ];

  if not (Sys.file_exists ".git") then begin
    command "git init"
  end;

  let oc = open_out "autoconf/configure.ac" in

  Printf.fprintf oc "#######################################################\n";
  Printf.fprintf oc "#                                                     #\n";
  Printf.fprintf oc "#               DO NOT EDIT THIS FILE                 #\n";
  Printf.fprintf oc "#                                                     #\n";
  Printf.fprintf oc "#  Use ocp-autoconf to generate this file from:       #\n";
  Printf.fprintf oc "#   * ocp-autoconf.config for the OCaml part          #\n";
  Printf.fprintf oc "#   * ocp-autoconf.ac for autoconf parts              #\n";
  Printf.fprintf oc "#                                                     #\n";
  Printf.fprintf oc "#######################################################\n";

  Printf.fprintf oc "AC_INIT(%s,%s)\n" !!project_name !!project_version;
  Printf.fprintf oc "CONFIGURE_ARGS=$*\n";
  Printf.fprintf oc "AC_COPYRIGHT(%s)\n" !!project_copyright;
  Printf.fprintf oc "OCAML_MINIMAL_VERSION=%s\n" !!ocaml_minimal_version;

  output_string oc (find_content "skeleton/autoconf/configure.ocaml");

  if !!ocaml_unsupported_version <> "" then begin
    Printf.fprintf oc "if test \"$VERSION_CHECK\" = \"yes\" ; then\n";
    Printf.fprintf oc "  AX_COMPARE_VERSION( [$OCAMLVERSION], [ge], [%s],\n" !!ocaml_unsupported_version;
    Printf.fprintf oc "     AC_MSG_ERROR([Your version of OCaml: $OCAMLVERSION is not yet supported]))\n";
    Printf.fprintf oc "fi\n";
  end;

  if !!need_ocamllex then begin
    Printf.fprintf oc "AC_PROG_OCAMLLEX\n";
  end;

  if !!need_ocamlyacc then begin
    Printf.fprintf oc "AC_PROG_OCAMLYACC\n";
  end;

  if !need_camlp4 then begin
    Printf.fprintf oc "AC_PROG_CAMLP4\n";
    Printf.fprintf oc "if test \"$CAMLP4\" = \"no\"; then\n";
    Printf.fprintf oc "   AC_MSG_ERROR([You must install OCaml package 'camlp4'])\n";
    Printf.fprintf oc "fi\n";
  end;

  if !need_ocamlbuild then begin
    Printf.fprintf oc "if test \"$OCAMLBUILD\" = \"no\"; then\n";
    Printf.fprintf oc "   AC_MSG_ERROR([You must install OCaml package 'ocamlbuild'])\n";
    Printf.fprintf oc "fi\n";
  end;

  if !need_ocamlfind then begin
    (* Printf.fprintf oc "AC_PROG_FINDLIB\n"; ALREADY DONE *)
    Printf.fprintf oc "if test \"$OCAMLFIND\" = \"no\"; then\n";
    Printf.fprintf oc "   AC_MSG_ERROR([You must install OCaml package 'ocamlfind'])\n";
    Printf.fprintf oc "fi\n";
  end;

  let to_ac package =
    let package = Bytes.of_string package in
    for i = 0 to Bytes.length package -1 do
      match Bytes.get package i with
      | '-' -> Bytes.set package i '_'
      | _ -> ()
    done;
    Bytes.to_string package
  in

  List.iter (fun tool ->
      let pkg = to_ac tool in
      Printf.fprintf oc "AC_CHECK_TOOL([OCAML_TOOL_%s],[%s],[no])\n"
        pkg tool;
      Printf.fprintf oc "if test \"$OCAML_TOOL_%s\" = \"no\"; then\n" pkg;
      Printf.fprintf oc "   AC_MSG_ERROR([Please install OCaml tool '%s'.])\n" tool;
      Printf.fprintf oc "fi\n";

    ) !!need_tools;

  List.iter (fun (package, version) ->
      let pkg = to_ac package in
      Printf.fprintf oc "AC_CHECK_OCAML_PKG(%s)\n" package;
      Printf.fprintf oc "if test \"$OCAML_PKG_%s\" = \"no\"; then\n" pkg;
      Printf.fprintf oc "   AC_MSG_ERROR([Please install OCaml package '%s'.])\n" package;
      Printf.fprintf oc "fi\n";
      match version with
      | None -> ()
      | Some version ->
        Printf.fprintf oc "  AX_COMPARE_VERSION( [$OCAML_PKG_%s_VERSION], [lt], [%s],\n" pkg version;
        Printf.fprintf oc "     AC_MSG_ERROR([Version %s of %s is needed]))\n" version pkg;

    ) need_packages;

  List.iter (fun package ->
      let pkg = to_ac package in
      Printf.fprintf oc "AC_CHECK_OCAML_PKG([%s])\n" package;
      Printf.fprintf oc
        "AC_ARG_ENABLE(lwt,  [  --disable-%s           to disable %s],\n"
        package package;
      Printf.fprintf oc "                    [OCAML_PKG_%s=no],[])\n" pkg;

      Printf.fprintf oc "if test \"$OCAML_PKG_%s\" = \"no\"; then\n" pkg;
      Printf.fprintf oc "   %s_ENABLED=false\n" pkg;
      Printf.fprintf oc "else\n";
      Printf.fprintf oc "   %s_ENABLED=true\n" pkg;
      Printf.fprintf oc "fi\n";
      Printf.fprintf oc "AC_SUBST(%s_ENABLED)\n" pkg;

    ) !!optional_packages;

  List.iter (fun modname ->
      Printf.fprintf oc "AC_CHECK_OCAML_MODULE(%s)\n" modname
    ) !!need_modules;

  if Sys.file_exists "ocp-autoconf.ac" then begin
    Printf.eprintf "Using %S\n%!" "ocp-autoconf.ac";
    output_string oc (FileString.read_file "ocp-autoconf.ac");
  end;

  let default_config_vars =
    (List.map (fun s -> s, None)
      [
        "CONFIGURE_ARGS";
      ]) @
    (List.map (fun s -> s, Some (String.lowercase s)) [
        "ROOTDIR";
        "prefix";
        "exec_prefix";
        "bindir";
        "libdir";
        "datarootdir";
        "mandir";
        "datadir";

        "ocamldir";
        "metadir";
        "PACKAGE_NAME";
        "PACKAGE_VERSION";
      ]) @
    (List.map (fun s ->
         s, Some ("conf_" ^ String.lowercase s)) [
           "OCAMLVERSION";
           "OCAMLC";
           "OCAMLOPT";
           "OCAMLDEP";
           "OCAMLMKTOP";
           "OCAMLMKLIB";
           "OCAMLDOC";
           "OCAMLLIB";
           "OCAMLBIN";
      ])

  in
  let config_vars =
    default_config_vars @
    (List.map (fun s -> s, Some ("extra_" ^ String.lowercase s)
              ) !!extra_config_vars)  in

  let config_vars =
    if !!need_ocamllex then
      ("OCAMLLEX", Some "conf_ocamllex") :: config_vars
    else config_vars in

  let config_vars =
    if !!need_ocamlyacc then
      ("OCAMLYACC", Some "conf_ocamlyacc") :: config_vars
    else config_vars in

  let config_vars =
    if !need_ocamlbuild then
      ("OCAMLBUILD", Some "conf_ocamlbuild") :: config_vars
    else config_vars in

  let config_vars =
    if !need_ocamlfind then
      ("OCAMLFIND", Some "conf_ocamlfind") :: config_vars
    else config_vars in

  let config_vars =
    if !need_camlp4 then
      ("CAMLP4", Some "conf_camlp4") ::
      ("CAMLP4O", Some "conf_camlp4o") ::
      config_vars
    else config_vars in

  List.iter (fun (var, _) ->
      Printf.fprintf oc "AC_SUBST(%s)\n" var
    ) config_vars;

  let bool_vars =
    "OCAML_USE_BINANNOT" ::
    !!extra_bool_vars
    @
    List.map (fun package ->
        let pkg = to_ac package in
        Printf.sprintf "%s_ENABLED" pkg
      ) !!optional_packages;
  in

  List.iter (fun var ->
      Printf.fprintf oc "AC_SUBST(%s)\n" var
    ) bool_vars;


  Printf.fprintf oc "AC_CONFIG_FILES(%s)\n"
    (String.concat " "
       ("Makefile.config" :: "config.ocpgen" :: "ocaml-config.h" ::
        !!extra_config_files));

  output_string oc (find_content "skeleton/autoconf/configure.trailer");
  close_out oc;


  let oc = open_out "autoconf/Makefile.config.in" in
  List.iter (fun (var,_) ->
      Printf.fprintf oc "%s=@%s@\n" var var;
    ) config_vars;
  List.iter (fun var ->
      Printf.fprintf oc "%s=@%s@\n" var var;
    ) bool_vars;
  close_out oc;

  close_out oc;

  let oc = open_out "autoconf/config.ocpgen.in" in
  List.iter (function
      | (var, None)-> ()
      | (var, Some name) ->
        Printf.fprintf oc "%s=\"@%s@\"\n" name var;
    ) config_vars;

  List.iter (fun var ->
      Printf.fprintf oc "%s = @%s@\n" (String.lowercase var) var
    ) bool_vars;

  Printf.fprintf oc "autoconf_dir = \"@PACKAGE_NAME@-autoconf-dir\"\n";
  output_string oc (find_content "skeleton/autoconf/config.trailer");
  close_out oc;


  let oc = open_out "autoconf/ocaml-config.h.in" in
  Printf.fprintf oc "#@OCAML_USE_POSIX_TYPES@ OCAML_USE_POSIX_TYPES\n";
  close_out oc;

  let oc = open_out "autoconf/build.ocp" in
  Printf.fprintf oc "(* Just here to refer to this directory *)\n";
  Printf.fprintf oc "if include \"config.ocpgen\" then {} else {}\n";
  Printf.fprintf oc "begin library autoconf_dir end\n";
  close_out oc;

  Unix.chdir "autoconf";

  command "aclocal -I m4";
  command "autoconf";

  Unix.chdir "..";

  if !arg_git_add then begin
    let files = !extra_files @ [
      "ocp-autoconf.ac";
      ".gitignore";
      "configure";
      "ocp-autoconf.config";
      "LICENSE";
      "build.ocp";
      "Makefile";
      "autoconf";
      "autoconf/config.ocpgen.in";
      "autoconf/.gitignore";
      "autoconf/configure";
      "autoconf/ocaml-config.h.in";
      "autoconf/Makefile.rules";
      "autoconf/configure.ac";
      "autoconf/build.ocp";
      "autoconf/Makefile.config.in";
      "autoconf/aclocal.m4";
      "autoconf/m4";
      "autoconf/m4/ax_compare_version.m4";
      "autoconf/m4/ocaml.m4";
    ] in
    let cmd = Printf.sprintf "git add %s" (String.concat " " files) in
    command cmd
  end;

  Printf.eprintf "Now, you should call ./configure\n%!"
