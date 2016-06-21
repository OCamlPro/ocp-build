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

let homedir = File.of_string AutoconfCommon.homedir

let config_file = File.add_basenames homedir
    [ ".ocp"; "ocp-autoconf"; "ocp-autoconf.conf" ]
let config = SimpleConfig.create_config_file config_file

let default_copyright = SimpleConfig.create_option config
    [ "default_copyright" ]
    [ "Project Copyright, if not specified" ]
    (SimpleConfig.option_option SimpleConfig.string_option)
    None

let opam_repo = SimpleConfig.create_option config
    [ "opam_repo" ]
    [ "Local copy of your fork of the OPAM public repository" ]
    SimpleConfig.string_option
    ""

let opam_repo_official_remote = SimpleConfig.create_option config
    [ "opam_repo_official_remote" ]
    [ "Name of GIT remote for the official OPAM repository" ]
    SimpleConfig.string_option
    ""

let opam_repo_fork_remote = SimpleConfig.create_option config
    [ "opam_repo_fork_remote" ]
    [ "Name of GIT remote for your fork of the OPAM repository" ]
    SimpleConfig.string_option
    ""

let format_version = SimpleConfig.create_option config
    [ "format_version" ]
    [ "Version of the format of this file" ]
    SimpleConfig.int_option
    0

let current_format_version = 2

let save () =
  File.safe_mkdir (File.dirname config_file);
  SimpleConfig.save_with_help config

let load () =

  begin
    try
      Printf.eprintf "Loading user config: %s ...%!"
        (File.to_string config_file);
      SimpleConfig.load config;
      Printf.eprintf "ok\n%!";
    with
    | SimpleConfig.LoadError (_, error) as exn ->
      Printf.eprintf "failed\n%!";
      begin
        match error with
        | SimpleConfig.FileDoesNotExist ->
          Printf.eprintf "Warning: %S does not exist.\n%!"
            (File.to_string config_file);
          Printf.eprintf "Generating a default version.\n%!";
          save ()
        | _ -> raise exn
      end
    | exn ->
      Printf.eprintf "failed\n%!";
      raise exn
  end;

  if !!format_version < current_format_version then begin
    format_version =:= current_format_version;
    Printf.eprintf "Updating %S to version %d\n%!"
      (File.to_string config_file) current_format_version;
    save ()
  end
