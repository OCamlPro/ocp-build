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
open AutoconfProjectConfig
open SimpleConfig.Op (* !! and =:= *)

let opam_version = "1.2"

let abort _oc =
  exit 2

let wrong_value oc field options =
  if List.mem field !!opam_fields then begin
    Printf.eprintf
      "Error: cannot set 'opam' field %S:\n" field;
    Printf.eprintf "You should set a correct value for \"%s\"\n"
      (String.concat "\" or \"" options);
    Printf.eprintf "or just remove %S from 'opam_fields'\n%!" field;
    Printf.eprintf "in the 'ocp-autoconf.config' file.\n";
    abort oc
  end

let opam_trailer = Filename.concat AutoconfArgs.ocp_autoconf_dir "opam.trailer"
let opam_trailer_old = "opam.trailer"

let () =

  AutoconfCommon.register_maker "opam" (fun () ->
      let opam_maintainer = !!AutoconfProjectConfig.opam_maintainer in

      let already_done = ref StringSet.empty in

      let oc = AutoconfFS.open_out "opam" in
      AutoconfFS.fprintf oc
        "(**************************************************************)\n";
        AutoconfFS.fprintf oc
          "(*                                                            *)\n";
        AutoconfFS.fprintf oc
          "(*      This file is managed by ocp-autoconf                  *)\n";
        AutoconfFS.fprintf oc
          "(*  Remove it from `manage_files` in 'ocp-autoconf.config'    *)\n";
        AutoconfFS.fprintf oc
          "(*  if you want to modify it manually (or use 'opam.trailer') *)\n";
        AutoconfFS.fprintf oc
          "(*                                                            *)\n";
        AutoconfFS.fprintf oc
          "(**************************************************************)\n";
        AutoconfFS.fprintf oc "\n";

      List.iter (fun field ->
          if not (StringSet.mem field !already_done) then
            let () =
              already_done := StringSet.add field !already_done
            in
            match field with

            | "opam-version" ->
              AutoconfFS.fprintf oc "opam-version: %S\n" opam_version

            | "build" ->
              AutoconfFS.fprintf oc "build: [\n";
              AutoconfFS.fprintf oc "  [ ";
              List.iter (fun cmd ->
                  AutoconfFS.fprintf oc "    %S\n" cmd)
                !!AutoconfProjectConfig.opam_configure_line;
              AutoconfFS.fprintf oc "  ]\n";
              AutoconfFS.fprintf oc "  [ make ]\n";
              AutoconfFS.fprintf oc "]\n";

            | "install" ->
              AutoconfFS.fprintf oc "install: [\n";
              AutoconfFS.fprintf oc "  [ make \"install\" ]\n";
              AutoconfFS.fprintf oc "]\n"

            | "remove" ->
              AutoconfFS.fprintf oc "remove: [\n";
              List.iter (fun cmd ->
                  AutoconfFS.fprintf oc "  [ ";
                  List.iter (fun cmd ->
                      AutoconfFS.fprintf oc "%S " cmd) cmd;
                  AutoconfFS.fprintf oc "  ]\n"
                )
                !!AutoconfProjectConfig.opam_remove_commands;
              List.iter (fun pkg ->
                  AutoconfFS.fprintf oc "  [ \"ocp-build\" \"uninstall\" %S ]\n" pkg)
                !!AutoconfProjectConfig.install_packages;
              AutoconfFS.fprintf oc "]\n"

            | "depends" ->
              let opam_deps = ref StringMap.empty in
              List.iter (fun pkg ->
                  match pkg.opam with
                  | Some "" | Some "-" -> ()
                  | _ ->
                    let name = match pkg.opam with
                      | None -> pkg.name
                      | Some name -> name
                    in
                    try
                      let old_version = StringMap.find name !opam_deps in
                      match old_version, pkg.version with
                      | None, None -> ()
                      | Some _, None -> ()
                      | None, Some _ ->
                        opam_deps := StringMap.add name pkg.version !opam_deps
                      | Some v1, Some v2 ->
                        if v1 <> v2 then begin
                          Printf.eprintf "Error: opam dep %S has inconsistent version bounds\n%!" name;
                          exit 2
                          end
                    with Not_found ->
                      opam_deps := StringMap.add name pkg.version !opam_deps
                ) !!AutoconfProjectConfig.need_packages;

              (* Add ocamlfind if we need to test for other packages being
                 installed before. *)
              if !opam_deps <> StringMap.empty &&
                 not (StringMap.mem "ocamlfind" !opam_deps) then begin
                opam_deps := StringMap.add "ocamlfind" None !opam_deps;
              end;

              AutoconfFS.fprintf oc "depends: [\n";
              StringMap.iter (fun name version ->
                    match version with
                    | None ->
                      AutoconfFS.fprintf oc "     %S\n" name
                    | Some v ->
                      AutoconfFS.fprintf oc "     %S {>= %S }\n" name v
                ) !opam_deps;
              AutoconfFS.fprintf oc "]\n";

            | "available" ->
              let ocaml_unsupported_version =
                !!AutoconfProjectConfig.ocaml_unsupported_version in
              if ocaml_unsupported_version <> "" then
                AutoconfFS.fprintf oc "available: [ocaml-version >= %S && ocaml-version < %S]\n"
                  !!AutoconfProjectConfig.ocaml_minimal_version
                  ocaml_unsupported_version
              else
                AutoconfFS.fprintf oc "available: [ocaml-version >= %S]\n"
                  !!AutoconfProjectConfig.ocaml_minimal_version

            | "maintainer" ->
              if opam_maintainer <> "" then
                AutoconfFS.fprintf oc "maintainer: %S\n" opam_maintainer
              else
                wrong_value oc field ["opam_maintainer"]

            | "authors" ->
              let authors = !!AutoconfProjectConfig.authors in
              if authors <> [] then begin
                AutoconfFS.fprintf oc "authors: [\n";
                List.iter (fun name ->
                    AutoconfFS.fprintf oc "  %S\n" name) authors;
                AutoconfFS.fprintf oc "]\n";
              end
              else
                wrong_value oc field ["authors"]

            | "homepage" ->
              let homepage = !!AutoconfProjectConfig.homepage in
              let homepage =
                if homepage = "" &&
                   !!github_project <> "" then
                  Printf.sprintf "http://github.com/%s" !!github_project
                else homepage
              in
              if homepage <> "" then
                AutoconfFS.fprintf oc "homepage: %S\n" homepage
              else
                wrong_value oc field ["homepage"]

            | "dev-repo" ->
              let dev_repo = !!AutoconfProjectConfig.dev_repo in
              let dev_repo = if dev_repo = "" && !!github_project <> "" then
                  Printf.sprintf "https://github.com/%s.git" !!github_project
                else dev_repo in
              if dev_repo <> "" then
                AutoconfFS.fprintf oc "dev-repo: %S\n"  dev_repo
              else
                wrong_value oc field ["dev_repo"; "github_project"]

            | "bug-reports" ->
              let bug_reports = !!AutoconfProjectConfig.bug_reports in
              let bug_reports = if bug_reports = "" && !!github_project <> "" then
                  Printf.sprintf "https://github.com/%s/issues" !!github_project
                else bug_reports in
              if bug_reports <> "" then
                AutoconfFS.fprintf oc "bug-reports: %S\n" bug_reports
              else
                wrong_value oc field ["bug_reports"; "github_project"]

            | _ ->
              Printf.eprintf "Error: no support for opam field %S\n%!" field;
              abort oc

        ) !!opam_fields;

      if Sys.file_exists opam_trailer_old then begin
        FileString.safe_mkdir AutoconfArgs.ocp_autoconf_dir;
        Sys.rename opam_trailer_old opam_trailer;
      end;
      if Sys.file_exists opam_trailer then begin
        Printf.eprintf "   using %S\n%!" opam_trailer;
        AutoconfFS.fprintf oc "\n";
        AutoconfFS.fprintf oc
          "(**************************************************************)\n";
        AutoconfFS.fprintf oc
          "(*                                                            *)\n";
        AutoconfFS.fprintf oc
          "(* From opam.trailer:                                         *)\n";
        AutoconfFS.fprintf oc
          "(*                                                            *)\n";
        AutoconfFS.fprintf oc
          "(**************************************************************)\n";
        AutoconfFS.fprintf oc "\n";
        AutoconfFS.output_string oc (FileString.read_file opam_trailer);
      end else begin
        Printf.eprintf "   no file %S\n%!" opam_trailer;
      end;
      AutoconfFS.close_out oc;
      ()
    )

let () =
  AutoconfCommon.register_maker "push-opam.sh" (fun () ->

    let descr_file = Filename.concat AutoconfArgs.ocp_autoconf_dir "descr" in
    let descr_file_old = "descr" in
    if not (Sys.file_exists descr_file) &&
      Sys.file_exists descr_file_old then begin
        Sys.rename descr_file_old descr_file
    end;

    let findlib_file =
      Filename.concat AutoconfArgs.ocp_autoconf_dir "findlib" in
    let findlib_file_old = "findlib" in
    if not (Sys.file_exists findlib_file) &&
      Sys.file_exists findlib_file_old then begin
        Sys.rename findlib_file_old findlib_file
      end;

    if not ( Sys.file_exists descr_file) then begin
      Printf.eprintf
        "Warning: file '%s' needs to be present for\n" descr_file;
      Printf.eprintf "  'push-opam.sh' to be executed.\n%!";
    end;
    if (!!AutoconfProjectConfig.download_url_prefix = "" &&
        !!AutoconfProjectConfig.github_project = "") ||
      !!AutoconfGlobalConfig.opam_repo = "" then begin

        Printf.eprintf
          "Warning: 'push-opam.sh' is not generated because:\n";
        if !!AutoconfProjectConfig.download_url_prefix = "" &&
             !!AutoconfProjectConfig.github_project = "" then
          Printf.eprintf "  * both 'download_url' and 'github_project' are empty (ocp-autoconf.config)\n";
        if !!AutoconfGlobalConfig.opam_repo = ""then
          Printf.eprintf "  * option 'opam_repo' is empty (~/.ocp/ocp-autoconf/)\n";

        Printf.eprintf "%!"

      end else begin

        AutoconfFS.write_file ~exe:true "push-opam.sh"
          (AutoconfCommon.find_content "skeleton/push-opam.sh");
      end
  )
