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

let (!!) = SimpleConfig.(!!)
let (=:=) = SimpleConfig.(=:=)

let homedir = FileGen.of_string AutoconfCommon.homedir

let config_file = FileGen.add_basenames homedir
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
  FileGen.safe_mkdir (FileGen.dirname config_file);
  SimpleConfig.save_with_help config

let load () =

  begin
    try
      Printf.eprintf "Loading user config: %s ...%!"
        (FileGen.to_string config_file);
      SimpleConfig.load config;
      Printf.eprintf "ok\n%!";
    with
    | SimpleConfig.LoadError (_, error) as exn ->
      Printf.eprintf "failed\n%!";
      begin
        match error with
        | SimpleConfig.FileDoesNotExist ->
          Printf.eprintf "Warning: %S does not exist.\n%!"
            (FileGen.to_string config_file);
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
      (FileGen.to_string config_file) current_format_version;
    save ()
  end
