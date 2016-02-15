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


(* ocp-build prefs [OPTIONS]

  Set the options of the user preference file.

*)

(* open BuildBase *)
open BuildArgs
open BuildOptions

let filename = ref UserOptions.default_filename
let arg_list =
  ("-f", Arg.String (fun s -> filename := s),
   "FILENAME Save preferences to file FILENAME") ::
    [
      arg_set_int UserOptions.njobs_option;
      arg_set_int UserOptions.verbosity_option;

      arg_set_true UserOptions.autoscan_option;
      arg_set_false UserOptions.autoscan_option;

      arg_set_true UserOptions.color_option;
      arg_set_false UserOptions.color_option;

      arg_set_true UserOptions.digest_option;
      arg_set_false UserOptions.digest_option;

      arg_set_true UserOptions.bytecode_option;
      arg_set_false UserOptions.bytecode_option;

      arg_set_true UserOptions.native_option;
      arg_set_false UserOptions.native_option;
    ]

let action () =
  BuildOptions.load_config UserOptions.config_file (File.of_string !filename);
  BuildOptions.apply_arguments ();
  BuildOptions.save_config UserOptions.config_file

let subcommand = {
  sub_name = "prefs";
  sub_help =  "Set the user global preferences.";
  sub_arg_list = arg_list;
  sub_arg_anon = None;
  sub_arg_usage = [
    "Set the user global preferences.";
  ];
  sub_action = action;
}

