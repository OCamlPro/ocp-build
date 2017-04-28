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
open AutoconfProjectConfig

let () =
  List.iter (fun file ->
      AutoconfCommon.register_maker file
        (fun () ->
           AutoconfCommon.save_file (Filename.concat "skeleton" file)))
    [
    "Makefile";
    "LICENSE";
    "autoconf/travis-install.sh";
    "autoconf/travis-ci.sh";
    ]

let () =
  List.iter (fun (file, template, user) ->
      AutoconfCommon.register_maker file
        (fun () ->
          let s = AutoconfCommon.find_content
            (Filename.concat "skeleton" template) in
          let oc = AutoconfFS.open_out file in
          AutoconfFS.output_string oc s;
          let user_file = Filename.concat AutoconfArgs.ocp_autoconf_dir user in
          if Sys.file_exists user_file then begin
            let s = FileString.read_file user_file in
            AutoconfFS.output_string oc s;
          end;
          AutoconfFS.close_out oc
        ))
    [
    "build.ocp", "build.ocp-tmpl", "build.ocp-inc";
    "build.ocp2", "build.ocp2-tmpl", "build.ocp2-inc";
    ".gitignore", "gitignore", "gitignore";
  ]



let () =
  AutoconfCommon.register_maker ".travis.yml"
    (fun () ->
       let s = AutoconfCommon.find_content "skeleton/.travis.yml" in
       let oc = AutoconfFS.open_out ".travis.yml" in
       AutoconfFS.output_string oc s;
       List.iter (fun version ->
           if version = "system" ||
              (version >= !!ocaml_minimal_version &&
               (!!ocaml_unsupported_version = "" ||
                !!ocaml_unsupported_version > version)) then
             AutoconfFS.output_string oc
               (Printf.sprintf "  - OCAML_VERSION=%s\n" version)
         ) !!travis_versions;
       AutoconfFS.close_out oc
    )
