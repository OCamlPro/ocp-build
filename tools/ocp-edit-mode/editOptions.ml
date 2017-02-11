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

open SimpleConfig

let must_save_global = ref false

let home_dir = try Sys.getenv "HOME" with Not_found -> "."
let home_dir = File.of_string home_dir
let config_dir = File.add_basename home_dir ".ocp"
let config_file = File.add_basename config_dir "ocp-edit-mode.conf"

let config = SimpleConfig.create_config_file config_file

(* the current version of this file. As soon as you add a new option,
   you should increase this value. *)
let current_update = 7

let install_directory = create_option config
  [ "install_directory" ] [ "Where ocp-edit-mode files are stored";
                            "Could be TYPEREX/tools/ocp-edit-mode/files";
                            "where TYPEREX is the directory with the";
                            "sources of TypeRex.";
                          ]
  string_option ""

let mli_directories = create_option config
  [ "mli_directories" ]
  [ "A list of directories containing .mli files";
    "that should be used for completion.";
  ]
  (list_option string_option) [ "/usr/lib/ocaml" ]

let emacs_major_mode = create_option config
  [ "emacs_major_mode" ] [
    "Major mode used by default for OCaml files";
    "(either \"tuareg-mode\" or \"caml-mode\")";
  ]
  string_option "tuareg-mode"

let debug_on_error = create_option config
  [ "debug_on_error" ] [
    "Stop configuration for debugging in case of error";
  ]
  bool_option false

let delete_trailing_whitespaces = create_option config
  [ "delete_trailing_whitespaces" ]
  [ "Delete trailing white-spaces at end of lines" ]
  bool_option true

let indent_use_tabs = create_option config
  [ "indent_use_tabs" ]
  [ "Set only if you want to use tabulations instead of spaces";
    "(\"None\" to use the system setting)" ]
  (option_option bool_option) (Some false)


let query_tools = create_option config
  [ "query_tools" ]
  [ "Preferred order for query tools (ocp-index,ocp-annot)" ]
  (list_option string_option)
  [ "ocp-index"; "ocp-annot" ]

let show_paren_style = create_option config
  [ "show_paren_style" ]
  [ "The style used to show parenthesis";
    "(\"parenthesis\" or \"expression\", or \"None\" to use the system setting)" ]
  (option_option string_option) None

let column_number_mode = create_option config
  [ "column_number_mode" ]
  [ "Show column numbers in the mode-line";
    "(\"None\" to use the system setting)" ]
  (option_option bool_option) None

let require_final_newline = create_option config
  [ "require_final_newline" ]
  [ "Ensure the file always ends with a newline";
    "(\"None\" to use the system setting)" ]
  (option_option bool_option) (Some true)

let auto_complete_key =  create_option config
  [ "auto_complete_key" ]
  [ "Key to use to trigger auto-completion";
    "(\"None\" disables the auto-completion which comes with TypeRex)" ]
  (option_option string_option) None (* (Some "TAB") *)

let abbrevs = create_option config
  [ "abbreviations" ]
  [ "Strings that are immediately replaced by other strings."]
  (list_option (tuple2_option (string_option, string_option)))

  [
    (* module List *)
    "ite'l", "List.iter";
    "len'l", "List.length";
    "rev'l", "List.rev";
    "fold_l", "List.fold_left";
    "fold_r", "List.fold_right";
    "map'l", "List.map";
    "mem'l", "List.mem";
    "memq'l", "List.memq";
    "mem_'l", "List.mem_assoc";
    "ass'l", "List.assq";
    "spl'l", "List.split";
    "com'l", "List.combine";

    (* module Array *)
    "ite'a", "Array.iter";
    "len'a", "Array.length";
    "set'a", "Array.set";
    "get'a", "Array.get";
    "cre'a", "Array.create";

    (* module Printf *)
    "pr'", "Printf.printf";
    "epr'", "Printf.eprintf";
    "fpr'", "Printf.fprintf";
    "bpr'", "Printf.bprintf";
    "spr'", "Printf.sprintf";

    (* module Hashtbl *)
    "ite'h", "Hashtbl.iter";
    "fin'h", "Hashtbl.find";
    "add'h", "Hashtbl.add";
    "rem'h", "Hashtbl.remove";
    "cre'h", "Hashtbl.create";
    "cle'h", "Hashtbl.clear";

    (* module Queue *)
    "ite'q", "Queue.iter";
    "len'q", "Queue.length";
    "cre'q", "Queue.create";
    "add'q", "Queue.add";
    "get'q", "Queue.take";
    "tak'q", "Queue.take";
    "cle'q", "Queue.clear";
    "emp'q", "Queue.Empty";

    (* module Filename *)
    "con'f", "Filename.concat";
    "che'f", "Filename.check_suffix";
    "cho'f", "Filename.chop_suffix";
    "bas'f", "Filename.basename";
    "dir'f", "Filename.dirname";

    (* module String *)
    "len's", "String.length";
    "set's", "String.set";
    "get's", "String.get";
    "cre's", "String.create";
    "sub's", "String.sub";
    "cop's", "String.copy";
    "bli's", "String.blit";
    "con's", "String.concat";
    "low's", "String.lowercase";
    "upp's", "String.uppercase";
    "cap's", "String.capitalize";
    "unc's", "String.uncapitalize";

    (* module Mutex *)
    "loc'm", "Mutex.lock";
    "unl'm", "Mutex.unlock";
    "cre'm", "Mutex.create";

    (* module Condition *)
    "cre'c", "Condition.create";
    "wai'c", "Condition.wait";

    (* module Thread *)
    "cre't", "Thread.create";

    (* module Weak *)
    "len'w", "Weak.length";
    "set'w", "Weak.set";
    "get'w", "Weak.get";

    (* module Char  *)
    "upp'c", "Char.uppercase";
    "low'c", "Char.lowercase";
  ]

let last_update = create_option config
  [ "last_update" ] [ "Do not modify" ] int_option 0

let load_or_create () =
  if File.exists config_file then begin
    try
      SimpleConfig.load config;
      if !!last_update < current_update then begin
        last_update =:= current_update;
        must_save_global := true
      end
    with e ->
      Printf.fprintf stderr "Error while loading %s\n"
        (File.to_string config_file);
      Printf.fprintf stderr "\tException %s\n%!"
        (Printexc.to_string e);
      exit 2
  end else begin
    Printf.eprintf "Warning: file %S does not exist. Creating with default values.\n%!" (File.to_string config_file);
    Dir.make_all config_dir;
    SimpleConfig.save_with_help config
  end

let save () =
  must_save_global := false;
  SimpleConfig.save_with_help config

let maybe_save () =
  if !must_save_global then save ()
