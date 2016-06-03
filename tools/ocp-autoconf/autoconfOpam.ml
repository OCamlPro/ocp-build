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
open AutoconfProjectConfig
open SimpleConfig.Op (* !! and =:= *)

let opam_version = "1.2"

let abort oc =
  close_out oc;
  Sys.remove "opam.tmp";
  exit 2

let wrong_value oc field options =
  if List.mem field !!opam_fields then begin
    Printf.eprintf
      "Error: cannot set 'opam' field %S:\n" field;
    Printf.eprintf "You should set a correct value for \"%s\"\n" options;
    Printf.eprintf "or just remove %S from 'opam_fields'\n%!" field;
    Printf.eprintf "in the 'ocp-autoconf.config' file.\n";
    abort oc
  end

let manage () =

  Printf.eprintf "Generating opam file...\n%!";
  let opam_maintainer = !!AutoconfProjectConfig.opam_maintainer in

  let already_done = ref StringSet.empty in

  let oc = open_out "opam.tmp" in
  List.iter (fun field ->
      if not (StringSet.mem field !already_done) then
        let () =
          already_done := StringSet.add field !already_done
        in
      match field with

      | "opam-version" ->
        Printf.fprintf oc "opam-version: %S\n" opam_version

      | "build" ->
          Printf.fprintf oc "build: [\n";
          Printf.fprintf oc "  [ \"./configure\"\n";
          Printf.fprintf oc "    \"--prefix\" \"%%{prefix}%%\"\n";
          Printf.fprintf oc "    \"--with-ocamldir\" \"%%{prefix}%%/lib\"\n";
          Printf.fprintf oc "    \"--with-metadir\" \"%%{prefix}%%/lib\"\n";
          Printf.fprintf oc "  ]\n";
          Printf.fprintf oc "  [ make ]\n";
          Printf.fprintf oc "]\n";

        | "install" ->
          Printf.fprintf oc "install: [\n";
          Printf.fprintf oc "  [ make \"install\" ]\n";
          Printf.fprintf oc "]\n"

        | "remove" ->
          Printf.fprintf oc "remove: [\n";
          List.iter (fun file ->
              Printf.fprintf oc "  [ \"rm\" \"-f\" %S ]\n" file)
            !!AutoconfProjectConfig.opam_remove_files;
          List.iter (fun pkg ->
              Printf.fprintf oc "  [ \"ocp-build\" \"uninstall\" %S ]\n" pkg)
            !!AutoconfProjectConfig.install_packages;
          Printf.fprintf oc "]\n"

        | "depends" ->
          Printf.fprintf oc "depends: [\n";
          List.iter (fun (n,v) ->
              match v with
              | None ->
                Printf.fprintf oc "     %S\n" n
              | Some v ->
                Printf.fprintf oc "     %S {>= %S }\n" n v
            ) !!AutoconfProjectConfig.need_packages;
          Printf.fprintf oc "]\n";

        | "available" ->
          let ocaml_unsupported_version =
            !!AutoconfProjectConfig.ocaml_unsupported_version in
          if ocaml_unsupported_version <> "" then
            Printf.fprintf oc "available: [ocaml-version >= %S && ocaml-version < %S]\n"
              !!AutoconfProjectConfig.ocaml_minimal_version
              ocaml_unsupported_version
          else
            Printf.fprintf oc "available: [ocaml-version >= %S]\n"
              !!AutoconfProjectConfig.ocaml_minimal_version

        | "maintainer" ->
          if opam_maintainer <> "" then
            Printf.fprintf oc "maintainer: %S\n" opam_maintainer
          else
            wrong_value oc field "opam_maintainer"

        | "authors" ->
          let authors = !!AutoconfProjectConfig.authors in
          if authors <> [] then begin
            Printf.fprintf oc "authors: [\n";
            List.iter (fun name ->
                Printf.fprintf oc "  %S\n" name) authors;
            Printf.fprintf oc "]\n";
          end
          else
            wrong_value oc field "authors"

        | "homepage" ->
          let homepage = !!AutoconfProjectConfig.homepage in
          if homepage <> "" then
            Printf.fprintf oc "homepage: %S\n" homepage
          else
            wrong_value oc field "homepage"

        | "dev-repo" ->
          let dev_repo = !!AutoconfProjectConfig.dev_repo in
          let dev_repo = if dev_repo = "" && !!github_project <> "" then
              Printf.sprintf "https://github.com/%s.git" !!github_project
            else dev_repo in
          if dev_repo <> "" then
            Printf.fprintf oc "dev-repo: %S\n"  dev_repo
          else
            wrong_value oc field "dev_repo"

        | "bug-reports" ->
          let bug_reports = !!AutoconfProjectConfig.bug_reports in
          let bug_reports = if bug_reports = "" && !!github_project <> "" then
              Printf.sprintf "https://github.com/%s/issues" !!github_project
            else bug_reports in
          if bug_reports <> "" then
            Printf.fprintf oc "bug-reports: %S\n" bug_reports
          else
            wrong_value oc field "bug_reports"

        | _ ->
          Printf.eprintf "Error: no support for opam field %S\n%!" field;
          abort oc

    ) [
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
  ];

  if Sys.file_exists "opam.trailer" then
    output_string oc (FileString.read_file "opam.trailer");
  close_out oc;

  if Sys.file_exists "opam" then begin
    Sys.rename "opam" "opam.old";
  end;
  Sys.rename "opam.tmp" "opam";

  if not ( Sys.file_exists "descr" && Sys.file_exists "findlib") then begin
    Printf.eprintf
      "Warning: files 'descr' and 'findlib' need to be present for\n";
    Printf.eprintf "  'push-opam.sh' to be generated.\n%!";
  end else
  if (!!AutoconfProjectConfig.download_url_prefix = "" &&
      !!AutoconfProjectConfig.github_project = "") ||
     !!AutoconfGlobalConfig.opam_repo = "" then begin

    Printf.eprintf
      "Warning: options 'download_url' (ocp-autoconf.config) and 'opam_repo' (~/.ocp/ocp-autoconf/) need to be set for\n";
    Printf.eprintf "  'push-opam.sh' to be generated.\n%!";

  end else begin

    FileString.write_file "push-opam.sh"
      (AutoconfCommon.find_content "skeleton/push-opam.sh");

  end;


  [ "opam" ]
