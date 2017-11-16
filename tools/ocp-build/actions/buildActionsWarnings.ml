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

open Ezcmd.Modules

open OcpCompat
open BuildOCPTypes


type print_warnings =
| PrintWarningsAlways
| PrintWarningsIfChanged
| PrintWarningsNever

let warnings_version = 1

let env_warnings_kind = "env"
let pj_warnings_kind = "project"

let arg_no_warnings_string = "--no-warnings"
let arg_warnings_fmt = format_of_string "--%s-warnings"
let arg_pj_warnings_string = Printf.sprintf arg_warnings_fmt pj_warnings_kind
let arg_env_warnings_string = Printf.sprintf arg_warnings_fmt env_warnings_kind
let arg_all_warnings_string = "--all-warnings"

let warnings_filename_fmt = format_of_string "%s/_obuild/%s_warnings.data"

let arg_print_env_warnings = ref PrintWarningsIfChanged
let arg_print_pj_warnings = ref PrintWarningsIfChanged

let arg_list =
  Arg.translate ~docs: "WARNING OPTIONS"
  [
  arg_no_warnings_string, Arg.Unit (fun () ->
    arg_print_pj_warnings := PrintWarningsNever;
    arg_print_env_warnings := PrintWarningsNever;
  ),
  " Print no warnings at all";
  arg_all_warnings_string, Arg.Unit (fun () ->
    arg_print_env_warnings := PrintWarningsAlways;
    arg_print_pj_warnings := PrintWarningsAlways;
  ),
  " Print all warnings, even if no changed";
  arg_env_warnings_string, Arg.Unit (fun () ->
    arg_print_env_warnings := PrintWarningsAlways;
  ),
  " Print env warnings, even if no changed";
  arg_pj_warnings_string, Arg.Unit (fun () ->
    arg_print_pj_warnings := PrintWarningsAlways;
  ),
  " Print project warnings, even if no changed";

]

let print_warning w = Printf.eprintf "%s%!" w

let print_warnings project_dir arg_print_warnings warnings_kind w =
  let warnings_filename =
    Printf.sprintf warnings_filename_fmt (FileGen.to_string project_dir)
      warnings_kind in
  let count = BuildWarnings.count w in
  if count = 0 then begin
    (try Sys.remove warnings_filename with _ -> ());
  end else begin
    if
        match arg_print_warnings with
        | PrintWarningsNever ->
        BuildWarnings.clear w;
        Printf.eprintf
          "Warning: %d %s warnings were not printed (remove %s)\n%!"
          count warnings_kind arg_no_warnings_string;
        false

      | PrintWarningsIfChanged ->
        let old_w =
          try
            let ic = open_in_bin warnings_filename in
            let (version : int) = input_value ic in
            if version <> warnings_version then raise Exit;
            let (w : BuildWarnings.set) = input_value ic in
            close_in ic;
            w
          with _ ->
            BuildWarnings.empty_set ()
        in
        let equal = BuildWarnings.equal w old_w in
        if equal then begin
          Printf.eprintf
            "Warning: %d old %s warnings were not printed (add %(%s%))\n%!"
            count warnings_kind arg_warnings_fmt warnings_kind
        end;
        not equal
      | PrintWarningsAlways ->
        true

        then begin
          Printf.eprintf "----- %d %s warnings -----\n" count warnings_kind;
          BuildWarnings.iter print_warning w
        end;
        let oc = open_out_bin warnings_filename in
        output_value oc warnings_version;
        output_value oc w;
        close_out oc;
    end

let set_default_is_always () =
  if !arg_print_env_warnings = PrintWarningsIfChanged then
    arg_print_env_warnings := PrintWarningsAlways;
  if !arg_print_pj_warnings = PrintWarningsIfChanged then
    arg_print_pj_warnings := PrintWarningsAlways;
  ()

let print_env_warnings project_dir w =
  print_warnings project_dir !arg_print_env_warnings env_warnings_kind w

let print_pj_warnings project_dir w =
  print_warnings project_dir !arg_print_pj_warnings pj_warnings_kind  w
