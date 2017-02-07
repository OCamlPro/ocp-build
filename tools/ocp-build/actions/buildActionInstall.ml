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

open StdlibArg
open StringCompat

open BuildTypes
open BuildArgs
open BuildTerm
open BuildActions

let do_install bc dest_dir _install_what projects _package_map =

  let install_dirs = ref StringSet.empty in
  List.iter (fun p ->
      let module P = (val p : Package) in
      install_dirs := StringSet.add (P.install_dir()) !install_dirs;
    ) projects;

  let install_dirs = StringSet.to_list !install_dirs in

  let projects_to_install_set = ref StringSet.empty in
  let projects_to_install_list = ref [] in
  let add_to_install p =
    let module P = (val p : Package) in
    let lib = P.info in
    if not (StringSet.mem lib.lib_name !projects_to_install_set) then begin
      projects_to_install_set :=
        StringSet.add lib.lib_name !projects_to_install_set;
      projects_to_install_list := p :: !projects_to_install_list;
    end
    in

  List.iter add_to_install projects;

  let already_installed =

    let state = BuildUninstall.init dest_dir install_dirs
    in
    List.filter
      (fun p ->
         let module P = (val p : Package) in
         (*         let lib = P.info in *)
         (*         lib.lib_install && *)
         BuildUninstall.is_installed state P.name)
      !projects_to_install_list
  in
  let bold s =
    if term.esc_ansi then Printf.sprintf "\027[1m%s\027[m" s else s
  in
  if already_installed <> [] then begin
    let names =  String.concat ", " (List.map (fun p ->
        let module P = (val p : Package) in
        bold P.name) already_installed) in
    if !BuildArgs.auto_uninstall then begin
      Printf.printf "Packages %s are already installed, removing first...\n"
        names;
      let state =
        BuildUninstall.init dest_dir install_dirs
      in
      List.iter
        (fun p ->
           let module P = (val p : Package) in
           BuildUninstall.uninstall state P.name
        )
        already_installed;
      BuildUninstall.finish state
    end else begin
      Printf.eprintf "Error: Packages %s are already installed.\n%!" names;
      BuildMisc.clean_exit 2
    end;
  end;


  let install_errors = ref 0 in
    let install_ok = ref 0 in

    List.iter (fun p ->
        let module P = (val p : Package) in
        (*        let lib = P.info in *)
        P.install ();
        incr install_ok
      )
      !projects_to_install_list;
    if !install_errors > 0 then begin
      if !install_ok = 0 then
        Printf.eprintf "Install completely failed\n%!"
      else
        Printf.eprintf
          "Install partially failed: %d/%d packages not installed"
          !install_errors (!install_errors + !install_ok);
      BuildMisc.clean_exit 2
    end

let arg_list =
  BuildOptions.merge
    [
      [
  "-install-bundle", Arg.String (fun _s ->
    Printf.eprintf "Warning: option -install-bundle is obsolete\n%!"
    ),
  "BUNDLE Install a bundle packages to uninstall all\n  packages at once";

      ];
      BuildActionBuild.arg_list
    ]



let action () =
  BuildActionBuild.make_build_targets := true;
  let (p, bc, projects, package_map) = BuildActionBuild.do_build () in

  let install_where = BuildOCamlInstall.install_where p.cin p.cout in
  let install_what = BuildOCamlInstall.install_what () in
  do_install bc install_where.BuildOCamlInstall.install_destdir
    install_what projects package_map;
  ()



let subcommand = {
  sub_name = "install";
  sub_help =  "Install the project.";
  sub_arg_list = arg_list;
  sub_arg_anon = Some BuildArgs.arg_anon;
  sub_arg_usage = [ "Install the project."; ];
  sub_action = action;
}
