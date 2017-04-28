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

(* clean all generated object files *)
val delete_file_or_directory : string -> unit
val time_step : string -> unit
val time_steps : (string * float) list ref

type project_info = {
  project_dir : FileGen.t;
  cin : BuildOptions.config_input;
  cout : BuildOCamlConfig.TYPES.config_output;
}

val load_project : BuildWarnings.set -> project_info
