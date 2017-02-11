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
open EditOptions

open Subcommands.TYPES

let env_PATH =
  try OcpString.split (Sys.getenv "PATH") ':' with _ -> []

let print_elist elist =
  Printf.printf "%s\n"  (String.concat "\n" elist)

let add_hook mode hook =
  [ Printf.sprintf "(add-hook '%s %s)" mode hook ]

let add_lambda_hook mode body =
  [ Printf.sprintf "(add-hook '%s '(lambda()" mode ]
  @
    (List.map (fun s -> Printf.sprintf "    %s" s) body)
  @
    [ "  ))" ]

let defun name args body =
  [
    Printf.sprintf "(defun %s (%s)" name args
  ]
  @
   (List.map (fun s -> Printf.sprintf "  %s" s) body)
  @    [ ")" ]

let message msg =
  [ Printf.sprintf "(message %s)" msg ]

let message_quoted msg = message (Printf.sprintf "\"%s\"" msg)

let exec_on_buffer arg =
  [
    "   (let ((ocp-local-name (buffer-file-name)))";
    "     (let ((ocp-local-buffer (current-buffer)))";
    "        (with-temp-buffer ";
    "           (insert \"(set-buffer ocp-local-buffer)\")";
    "           (insert";
    Printf.sprintf "             (shell-command-to-string (concat \"ocp-edit-mode emacs %s '\" ocp-local-name \"'\"))) (eval-buffer))" arg;
    "     )";
    "   )";
  ]

let prepend_to_list list_name v =
  [ Printf.sprintf "(setq %s " list_name;
    Printf.sprintf "   (cons %s" v;
    Printf.sprintf "       %s))" list_name;
  ]

let setq name body =
  [ Printf.sprintf "(setq %s" name ] @
    List.map (fun s -> Printf.sprintf "  %s" s) body @
    [ ")" ]

let if_set opt f = match !!opt with
  | Some o -> f o
  | None -> ""

let if_set_else opt f e = match !!opt with
  | Some o -> f o
  | None   -> e

let has_tool tool =
  try
    Some (DirPath.find_in_path env_PATH tool)
  with _ -> None

let has_ocp_indent = has_tool "ocp-indent"
let has_ocp_index = has_tool "ocp-index"
let has_ocp_annot = has_tool "ocp-annot"
let has_ocamlspot = has_tool "ocamlspot"
let has_merlin = has_tool "merlin"

let load_global_config () =
  let install_directory =
    if !!install_directory = "" then
      Filename.concat EditVersion.datadir "ocp-edit-mode"
    else !!install_directory
  in
  print_elist
    (
      [
        Printf.sprintf "(setq debug-on-error %s)"
          (if !!debug_on_error then "t" else "nil")
      ] @
      (message_quoted "ocp-edit-mode global config loaded") @
        prepend_to_list "load-path"
        (Printf.sprintf  "\"%s/emacs\"" install_directory) @
        (List.map (fun tool ->
          match tool with
          | None -> ""
          | Some filename ->
            let prefix = Filename.dirname (Filename.dirname filename) in
            Printf.sprintf
              "(add-to-list 'load-path \"%s/share/emacs/site-lisp\")"
              prefix
         ) [ has_ocp_indent; has_ocp_annot ]
        )
        @
        defun "typerex-mode" ""
        (
          message "(concat \"typerex-mode called for \" (buffer-name))" @
            exec_on_buffer "-load-local-config"
        ) @ [
          if_set auto_complete_key (fun _ ->
              "(autoload 'auto-complete-mode \"auto-complete\" \"Minor mode for \
               completion\" t)")
        ] @ [
          "(autoload 'tuareg-mode \"tuareg\" \"Major mode for editing Caml code\" t)";
          "(autoload 'caml-mode \"caml\" \"Major mode for editing OCaml code.\" t)";
          (*          "(autoload 'ocp-fix-errors \"ocp-fix-errors.el\" \"Auto fix erros.\" t)"; *)
          "(require 'font-lock)";

          "(require 'paren)";

          (*
          "(setq ocamlspot-command \"ocp-spotter\")";
          "(setq ocamlspot-debug t)";
          "(require 'ocamlspot)";
          *)

        ] @

        prepend_to_list "auto-mode-alist"
        "'(\"\\\\.ml[iylp]?$\" . typerex-mode)" @
        prepend_to_list "auto-mode-alist"
        "'(\"\\.ocp$\" . typerex-mode)" @

        [
          match has_ocp_index with
          | Some _ ->
            List.assoc "files/ocp-index.el" EditFiles.files
          | None ->
            match has_ocp_annot with
            | Some _ ->
              List.assoc "files/ocp-annot.el" EditFiles.files
            | None -> ""
        ]

        @
        [ "(setq save-abbrevs nil)"    ] @
        add_lambda_hook "tuareg-mode-hook"
        (exec_on_buffer "-tuareg-mode-hook")    @
        add_lambda_hook "caml-mode-hook"
        (exec_on_buffer "-caml-mode-hook")    @
        [
          "(setq completion-ignored-extensions";
          "  (append completion-ignored-extensions";
          "    '(\".o\") '(\"~\") '(\".cmo\") '(\".cmi\") '(\".cma\")";
          "    '(\".cmx\") '(\".cmxa\") '(\".cmxs\") '(\".cmt\") '(\".cmti\")";
          "  ))";
        ]

    )

let load_local_config filename =
  Printf.printf "%s\n"
    (String.concat "\n"
       [
         "(message \"ocp-edit-mode local config loaded\")";
         "(set-buffer ocp-local-buffer)";
         Printf.sprintf "(%s)" !!emacs_major_mode;
       ]);
()

let lisp_bool () = function
  | true ->  "t"
  | false -> "nil"

let all_mode_hook mode =
  print_elist (
    (*
    [
      "(local-set-key  (kbd \"C-c C-f\") 'ocp-fix-errors)";
      "(local-set-key  (kbd \"C-c C-d\") 'ocp-fix-errors)";
    ]
    @
[
  "(local-set-key \"\\C-c;\" 'ocamlspot-query)";
  "(local-set-key \"\\C-c:\" 'ocamlspot-query-interface)";
  "(local-set-key \"\\C-c'\" 'ocamlspot-query-uses)";
  "(local-set-key \"\\C-c\\C-t\" 'ocamlspot-type)";
  "(local-set-key \"\\C-c\\C-i\" 'ocamlspot-xtype)";
  "(local-set-key \"\\C-c\\C-y\" 'ocamlspot-type-and-copy)";
  "(local-set-key \"\\C-cx\" 'ocamlspot-expand)";
  "(local-set-key \"\\C-c\\C-u\" 'ocamlspot-use)";
  "(local-set-key \"\\C-ct\" 'caml-types-show-type)";
  "(local-set-key \"\\C-cp\" 'ocamlspot-pop-jump-stack)";
]
      @
    *)
                          (match has_ocp_index with
                          | Some _ -> ["(ocp-index-mode t)"]
                          | None ->
                            match has_ocp_annot with
                          | Some _ -> ["(ocp-annot-mode t)"]
                          | None -> []
                          )@

      [
      "(show-paren-mode t)";
      "(setq blink-matching-paren t)";
      "(setq blink-matching-paren-on-screen t)";
      "(make-variable-buffer-local 'show-paren-style)";
      if_set show_paren_style
        (Printf.sprintf "(setq show-paren-style '%s)");
      "(setq blink-matching-paren-dont-ignore-comments t)";
      "(font-lock-mode t)";
      "(setq font-lock-maximum-decoration t)";
      if_set column_number_mode
        (Printf.sprintf "(setq column-number-mode %a)" lisp_bool);
      if_set require_final_newline
        (Printf.sprintf "(setq require-final-newline %a)" lisp_bool);
      ] @
      (if_set_else auto_complete_key (fun _ ->
        [
          "(require 'auto-complete-config)";
          "(defun ocp-candidates()";
          "   (let ((ocp-local-name (buffer-file-name)))";
          "     (let (result)";
          "      (with-temp-buffer ";
          "        (insert";
          "          (shell-command-to-string";
          "            (concat \"ocp-edit-mode candidates -infile \" ocp-local-name";
          "              \" '\" ac-prefix \"'\")))";
          "        (eval-buffer))";
          "      result)";
          "     )";
            "   )";
        ])
        []
      ) @ [
      "(defun ocp-documentation(candidate)";
      "   (let ((ocp-local-name (buffer-file-name)))";
      "     (let (result)";
      "      (with-temp-buffer ";
      "        (insert";
      "          (shell-command-to-string";
      "            (concat \"ocp-edit-mode documentation -infile \" ocp-local-name";
      "              \" '\" candidate \"'\")))";
      "        (eval-buffer))";
      "      result)";
      "     )";
      "   )";

      "(defun ocp-prefix-longident ()";
      "(let ((regexp \"[^a-zA-Z0-9'._]+\"))";
      "  (message (concat \"ocp-prefix-longident of \" regexp))";
      "  (let ((point (re-search-backward regexp nil t)))";
      (* completion does not work if the identifier starts at the beginning
         of the file *)
      "  (if point (1+ point)))))";

      ] @
        if_set_else auto_complete_key (fun k ->
            [
              "(ac-define-source ocp-complete";
              "  '((candidates . ocp-candidates)";
              "    (prefix . ocp-prefix-longident)";
              "    (document . ocp-documentation)";
              "    ))";
              "(setq ac-sources '(ac-source-ocp-complete))";
              "(setq ac-auto-start nil)";
              (Printf.sprintf "(ac-set-trigger-key \"%s\")" k);
              "(auto-complete-mode)";
            ])
          []
      @ [
      if_set indent_use_tabs
        (Printf.sprintf "(setq indent-tabs-mode %a)" lisp_bool);
      ] @
      List.map (fun (s1,s2) ->
        Printf.sprintf "  (define-abbrev %s-abbrev-table %S %S)" mode
          s1 s2) !!abbrevs @
      ["(abbrev-mode 1)" ]    @
      (if !!delete_trailing_whitespaces then
          [
            "(add-hook 'write-contents-hooks 'delete-trailing-whitespace)";
            "(setq show-trailing-whitespace t)";

          ]
       else [])
  )

let tuareg_mode_hook () =
  Printf.printf "%s\n"
    (String.concat "\n"
       [
         "(message \"loading tuareg-mode-hook...\")";
         "(set-buffer ocp-local-buffer)";
       ]);
  all_mode_hook "tuareg-mode";
  Printf.printf "%s\n"
    (String.concat "\n"
       [
         "(message \"loading tuareg-mode-hook done.\")";
       ]);
  ()

let caml_mode_hook () =
  Printf.printf "%s\n"
    (String.concat "\n"
       [
         "(message \"loading caml-mode-hook...\")";
         "(set-buffer ocp-local-buffer)";
       ]);
  all_mode_hook "caml-mode";

  Printf.printf "(require 'caml-font)\n";
  Printf.printf "(caml-font-set-font-lock)\n";
  Printf.printf "%s\n"
    (String.concat "\n"
       [
         "(message \"loading caml-mode-hook done.\")";
       ]);
  ()

let arg_list = [
  "-load-global-config", Arg.Unit load_global_config,
  " : load global Emacs config";
  "-load-local-config", Arg.String load_local_config,
  " FILENAME  : load local Emacs config for file";
  "-tuareg-mode-hook", Arg.Unit tuareg_mode_hook,    "";
  "-caml-mode-hook", Arg.Unit caml_mode_hook,    "";
]

let subcmd_spec = {
  subcmd_list = arg_list;
  subcmd_usage = [];
  subcmd_help = [];
}

let subcmd_main args = ()
let subcmd_init () = ()
