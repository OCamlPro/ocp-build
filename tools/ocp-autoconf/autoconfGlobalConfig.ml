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
open AutoconfArgs

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

let load () =

  begin
    try
      SimpleConfig.load config
    with
    | SimpleConfig.LoadError (_, error) as exn ->
      begin
        match error with
        | SimpleConfig.FileDoesNotExist ->
          Printf.eprintf "Warning: %S does not exist.\n%!"
            (File.to_string config_file);
          Printf.eprintf "Generating a default version.\n%!";
          File.safe_mkdir (File.dirname config_file);
          SimpleConfig.save_with_help config;
        | _ -> raise exn
      end
  end
