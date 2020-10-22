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
open AutoconfTypes

module PROJECT = AutoconfProjectConfig

let () =
  AutoconfCommon.register_maker "configure"
    (fun () ->
       AutoconfCommon.save_file ~exe:true "skeleton/configure";
    )

let () =
  AutoconfCommon.register_maker "autoconf"
    (fun () ->
       let extra_files = ref [] in

       let need_pkgs = ref [] in
       let need_ocamlbuild = ref false in
       let need_ocamlfind = ref false in
       let need_camlp4 = ref false in

       List.iter (function { name } as pkg ->
           match name with
           | "ocamlbuild" -> need_ocamlbuild := true
           | "ocamlfind" -> need_ocamlfind := true
           | "camlp4" -> need_camlp4 := true
           | _ ->
             need_pkgs := pkg :: !need_pkgs
         ) !!PROJECT.need_packages;
       if !!PROJECT.optional_packages <> [] || !!PROJECT.need_packages <> [] then
         need_ocamlfind := true;

       let need_packages = List.rev !need_pkgs in

       (*  FileString.safe_mkdir "autoconf";
           Printf.eprintf "Saving template files...\n%!"; *)
       List.iter AutoconfCommon.save_file [
         "skeleton/autoconf/m4/ax_compare_version.m4";
         "skeleton/autoconf/m4/ocaml.m4";
         "skeleton/autoconf/Makefile.rules";
         "skeleton/autoconf/.gitignore";
       ] ;
       List.iter (fun filename ->
           let content = FileString.read_file filename in
           let basename = Filename.basename filename in
           let dst_filename = Filename.concat "autoconf/m4" basename in
           extra_files := dst_filename :: !extra_files;
           AutoconfFS.write_file dst_filename content
         ) !!PROJECT.extra_m4_files;

       let oc = AutoconfFS.open_out "autoconf/configure.ac" in

       AutoconfFS.fprintf oc "#######################################################\n";
       AutoconfFS.fprintf oc "#                                                     #\n";
       AutoconfFS.fprintf oc "#               DO NOT EDIT THIS FILE                 #\n";
       AutoconfFS.fprintf oc "#                                                     #\n";
       AutoconfFS.fprintf oc "#  Use ocp-autoconf to generate this file from:       #\n";
       AutoconfFS.fprintf oc "#   * ocp-autoconf.config for the OCaml part          #\n";
       AutoconfFS.fprintf oc "#   * ocp-autoconf.ac for autoconf parts              #\n";
       AutoconfFS.fprintf oc "#                                                     #\n";
       AutoconfFS.fprintf oc "#######################################################\n";

       AutoconfFS.fprintf oc "AC_INIT(%s,%s)\n" !!PROJECT.project_name !!PROJECT.project_version;
       AutoconfFS.fprintf oc "CONFIGURE_ARGS=$*\n";
       AutoconfFS.fprintf oc "AC_COPYRIGHT(%s)\n" !!PROJECT.project_copyright;
       AutoconfFS.fprintf oc "OCAML_MINIMAL_VERSION=%s\n" !!PROJECT.ocaml_minimal_version;

       AutoconfFS.output_string oc
         (AutoconfCommon.find_content "skeleton/autoconf/configure.ocaml");

       if !!PROJECT.ocaml_unsupported_version <> "" then begin
         AutoconfFS.fprintf oc "if test \"$VERSION_CHECK\" = \"yes\" ; then\n";
         AutoconfFS.fprintf oc "  AX_COMPARE_VERSION( [$OCAMLVERSION], [ge], [%s],\n" !!PROJECT.ocaml_unsupported_version;
         AutoconfFS.fprintf oc "     AC_MSG_ERROR([Your version of OCaml: $OCAMLVERSION is not yet supported]))\n";
         AutoconfFS.fprintf oc "fi\n";
       end;

       if !!PROJECT.need_ocamllex then begin
         AutoconfFS.fprintf oc "AC_PROG_OCAMLLEX\n";
       end;

       if !!PROJECT.need_ocamlyacc then begin
         AutoconfFS.fprintf oc "AC_PROG_OCAMLYACC\n";
       end;

       if !need_camlp4 then begin
         AutoconfFS.fprintf oc "AC_PROG_CAMLP4\n";
         AutoconfFS.fprintf oc "if test \"$CAMLP4\" = \"no\"; then\n";
         AutoconfFS.fprintf oc "   AC_MSG_ERROR([You must install OCaml package 'camlp4'])\n";
         AutoconfFS.fprintf oc "fi\n";
       end;

       if !need_ocamlbuild then begin
         AutoconfFS.fprintf oc "if test \"$OCAMLBUILD\" = \"no\"; then\n";
         AutoconfFS.fprintf oc "   AC_MSG_ERROR([You must install OCaml package 'ocamlbuild'])\n";
         AutoconfFS.fprintf oc "fi\n";
       end;

       if !need_ocamlfind then begin
         (* AutoconfFS.fprintf oc "AC_PROG_FINDLIB\n"; ALREADY DONE *)
         AutoconfFS.fprintf oc "if test \"$OCAMLFIND\" = \"no\"; then\n";
         AutoconfFS.fprintf oc "   AC_MSG_ERROR([You must install OCaml package 'ocamlfind'])\n";
         AutoconfFS.fprintf oc "fi\n";
       end;

       let to_ac package =
         let package = Bytes.of_string package in
         for i = 0 to Bytes.length package -1 do
           match Bytes.get package i with
           | '-' -> Bytes.set package i '_'
           | '.' -> Bytes.set package i '_'
           | _ -> ()
         done;
         Bytes.to_string package
       in

       List.iter (fun tool ->
           let pkg = to_ac tool in
           AutoconfFS.fprintf oc "AC_CHECK_TOOL([OCAML_TOOL_%s],[%s],[no])\n"
             pkg tool;
           AutoconfFS.fprintf oc "if test \"$OCAML_TOOL_%s\" = \"no\"; then\n" pkg;
           AutoconfFS.fprintf oc "   AC_MSG_ERROR([Please install OCaml tool '%s'.])\n" tool;
           AutoconfFS.fprintf oc "fi\n";

         ) !!PROJECT.need_tools;

       let inverse_op = function
         | EqVersion -> "ne"
         | GeVersion -> "lt"
         | GtVersion -> "le"
         | LeVersion -> "gt"
         | LtVersion -> "ge"
       in
       List.iter (fun pkg ->
           let ac_pkg = to_ac pkg.name in
           AutoconfFS.fprintf oc "AC_CHECK_OCAML_PKG(%s)\n" pkg.name;
           AutoconfFS.fprintf oc "if test \"$OCAML_PKG_%s\" = \"no\"; then\n" ac_pkg;
           AutoconfFS.fprintf oc "   AC_MSG_WARN([BEFORE ERROR: Please install OCaml package '%s'.])\n" pkg.name;

           begin
           match pkg.version with
           | None -> ()
           | Some (op, version) ->
              AutoconfFS.fprintf oc "else\n";

                 AutoconfFS.fprintf oc "  AX_COMPARE_VERSION( [$OCAML_PKG_%s_VERSION], [%s], [%s],\n"  ac_pkg (inverse_op op) version;
                 AutoconfFS.fprintf oc "     AC_MSG_WARN([BEFORE ERROR: Version %s %s of %s is needed]))\n" (string_of_version_op op) version pkg.name;
                 end;
           AutoconfFS.fprintf oc "fi\n";
         ) need_packages;

       List.iter (fun pkg ->
           let ac_pkg = to_ac pkg.name in
           AutoconfFS.fprintf oc "if test \"$OCAML_PKG_%s\" = \"no\"; then\n" ac_pkg;
           AutoconfFS.fprintf oc "   AC_MSG_FAILURE([Missing dependencies (see warnings above).])\n";
           AutoconfFS.fprintf oc "fi\n";
           match pkg.version with
           | None -> ()
           | Some (op, version) ->
                 AutoconfFS.fprintf oc "  AX_COMPARE_VERSION( [$OCAML_PKG_%s_VERSION], [%s], [%s],\n"  ac_pkg (inverse_op op) version;
                 AutoconfFS.fprintf oc "     AC_MSG_FAILURE([Problems with dependencies (see warnings above)]))\n"

         ) need_packages;

       List.iter (fun package ->
           let pkg = to_ac package in
           AutoconfFS.fprintf oc "AC_CHECK_OCAML_PKG([%s])\n" package;
           AutoconfFS.fprintf oc
             "AC_ARG_ENABLE(lwt,  [  --disable-%s           to disable %s],\n"
             package package;
           AutoconfFS.fprintf oc "                    [OCAML_PKG_%s=no],[])\n" pkg;

           AutoconfFS.fprintf oc "if test \"$OCAML_PKG_%s\" = \"no\"; then\n" pkg;
           AutoconfFS.fprintf oc "   %s_ENABLED=false\n" pkg;
           AutoconfFS.fprintf oc "else\n";
           AutoconfFS.fprintf oc "   %s_ENABLED=true\n" pkg;
           AutoconfFS.fprintf oc "fi\n";
           AutoconfFS.fprintf oc "AC_SUBST(%s_ENABLED)\n" pkg;

         ) !!PROJECT.optional_packages;

       List.iter (fun modname ->
           AutoconfFS.fprintf oc "AC_CHECK_OCAML_MODULE(%s)\n" modname
         ) !!PROJECT.need_modules;

       AutoconfFS.fprintf oc "OPAM_REPO=%s\n"
         !!AutoconfGlobalConfig.opam_repo;
       AutoconfFS.fprintf oc "OPAM_REPO_OFFICIAL_REMOTE=%s\n"
         !!AutoconfGlobalConfig.opam_repo_official_remote;
       AutoconfFS.fprintf oc "OPAM_REPO_FORK_REMOTE=%s\n"
         !!AutoconfGlobalConfig.opam_repo_fork_remote;
       if !!PROJECT.download_url_prefix <> "" then
         AutoconfFS.fprintf oc "DOWNLOAD_URL_PREFIX=%s\n"
           !!PROJECT.download_url_prefix
       else
         AutoconfFS.fprintf oc "DOWNLOAD_URL_PREFIX=http://github.com/%s/archive/\n"
           !!PROJECT.github_project;

       let configure_ac_old = "ocp-autoconf.ac" in
       let configure_ac = Filename.concat
         AutoconfArgs.ocp_autoconf_dir "configure.ac" in
       FileString.safe_mkdir AutoconfArgs.ocp_autoconf_dir;
       if Sys.file_exists configure_ac_old then begin
         Sys.rename configure_ac_old configure_ac;
       end;

       if Sys.file_exists configure_ac then begin
         Printf.eprintf "  using %S\n%!" configure_ac;
        AutoconfFS.fprintf oc "\n";
        AutoconfFS.fprintf oc
          "###############################################################\n";
        AutoconfFS.fprintf oc
          "##                                                            #\n";
        AutoconfFS.fprintf oc
          "## From autoconf.ac:                                          #\n";
        AutoconfFS.fprintf oc
          "##                                                            #\n";
        AutoconfFS.fprintf oc
          "###############################################################\n";
        AutoconfFS.fprintf oc "\n";

        AutoconfFS.output_string oc (FileString.read_file configure_ac);
        AutoconfFS.fprintf oc "\n";
        AutoconfFS.fprintf oc
          "###############################################################\n";
        AutoconfFS.fprintf oc
          "##                                                            #\n";
        AutoconfFS.fprintf oc
          "## END of autoconf.ac                                         #\n";
        AutoconfFS.fprintf oc
          "##                                                            #\n";
        AutoconfFS.fprintf oc
          "###############################################################\n";
        AutoconfFS.fprintf oc "\n";

       end else begin
         Printf.eprintf "  no file %S; Creating one.\n%!" configure_ac;
         let file = configure_ac in
         AutoconfCommon.save_file (Filename.concat "skeleton" file)
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
             "OPAM_REPO";
             "OPAM_REPO_OFFICIAL_REMOTE";
             "OPAM_REPO_FORK_REMOTE";
             "DOWNLOAD_URL_PREFIX";

           ]) @
         (List.map (fun s ->
              s, Some ("conf_" ^ String.lowercase s)) [
             "OCAMLVERSION";
             "OCAMLVERSION_C";
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
         (List.map (fun s -> s, Some ("conf_" ^ String.lowercase s)
                   ) !!PROJECT.extra_config_vars)  in

       let config_vars =
         if !!PROJECT.need_ocamllex then
           ("OCAMLLEX", Some "conf_ocamllex") :: config_vars
         else config_vars in

       let config_vars =
         if !!PROJECT.need_ocamlyacc then
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
           AutoconfFS.fprintf oc "AC_SUBST(%s)\n" var
         ) config_vars;

       let bool_vars =
         "OCAML_USE_BINANNOT" ::
         !!PROJECT.extra_bool_vars
         @
         List.map (fun package ->
             let pkg = to_ac package in
             Printf.sprintf "%s_ENABLED" pkg
           ) !!PROJECT.optional_packages;
       in

       List.iter (fun var ->
           AutoconfFS.fprintf oc "AC_SUBST(%s)\n" var
         ) bool_vars;


       AutoconfFS.fprintf oc "AC_CONFIG_FILES(%s)\n"
         (String.concat " "
            ("Makefile.config" ::
             "config.ocpgen" ::
             "config.ocp2gen" ::
             "ocaml-config.h" ::
             !!PROJECT.extra_config_files));

       AutoconfFS.output_string oc
         (AutoconfCommon.find_content "skeleton/autoconf/configure.trailer");
       AutoconfFS.close_out oc;


       let oc = AutoconfFS.open_out "autoconf/Makefile.config.in" in
       List.iter (fun (var,_) ->
           AutoconfFS.fprintf oc "%s=@%s@\n" var var;
         ) config_vars;
       List.iter (fun var ->
           AutoconfFS.fprintf oc "%s=@%s@\n" var var;
         ) bool_vars;
       AutoconfFS.close_out oc;

       AutoconfFS.close_out oc;


       let oc = AutoconfFS.open_out "autoconf/config.ocpgen.in" in
       List.iter (function
           | (_var, None)-> ()
           | (var, Some name) ->
             AutoconfFS.fprintf oc "%s=\"@%s@\"\n" name var;
         ) config_vars;

       List.iter (fun var ->
           AutoconfFS.fprintf oc "%s = @%s@\n" (String.lowercase var) var
         ) bool_vars;

       AutoconfFS.fprintf oc "autoconf_dir = \"@PACKAGE_NAME@-autoconf-dir\"\n";
       AutoconfFS.output_string oc (AutoconfCommon.find_content
                                      "skeleton/autoconf/config.ocpgen.trailer");
       AutoconfFS.close_out oc;

       let oc = AutoconfFS.open_out "autoconf/config.ocp2gen.in" in

       AutoconfFS.output_string oc
         (AutoconfCommon.find_content
            "skeleton/autoconf/config.ocp2gen.header");

       List.iter (function
           | (_var, None)-> ()
           | (var, Some name) ->
             AutoconfFS.fprintf oc "  %s=\"@%s@\";\n" name var;
         ) config_vars;

       List.iter (fun var ->
           AutoconfFS.fprintf oc "  %s = @%s@;\n" (String.lowercase var) var
         ) bool_vars;

       AutoconfFS.fprintf oc "  autoconf_dir = \"@PACKAGE_NAME@-autoconf-dir\";\n";
       AutoconfFS.output_string oc
         (AutoconfCommon.find_content
            "skeleton/autoconf/config.ocp2gen.trailer");
       AutoconfFS.close_out oc;


       let oc = AutoconfFS.open_out "autoconf/ocaml-config.h.in" in
       AutoconfFS.fprintf oc "#@OCAML_USE_POSIX_TYPES@ OCAML_USE_POSIX_TYPES\n";
       AutoconfFS.close_out oc;

       (*
       let oc = AutoconfFS.open_out "autoconf/build.ocp" in
       AutoconfFS.fprintf oc "(* Just here to refer to this directory *)\n";
       AutoconfFS.fprintf oc "if include \"config.ocpgen\" then {} else {}\n";
       AutoconfFS.fprintf oc "begin library autoconf_dir end\n";
       AutoconfFS.close_out oc;
*)

       AutoconfFS.add_post_commit_hook (fun () ->
           Unix.chdir "autoconf";
           AutoconfCommon.command "aclocal -I m4";
           AutoconfCommon.command "autoconf";
           Unix.chdir "..";
           Printf.eprintf "Now, you should call ./configure\n%!";
           [ "autoconf/configure"; "autoconf/aclocal.m4"  ]
         )
    (*

  !extra_files @ [
      "ocp-autoconf.ac";
      ".gitignore";
      "configure";
      "ocp-autoconf.config";
      "LICENSE";
      "build.ocp";
      "build.ocp2";
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
  ]
*)
    )
