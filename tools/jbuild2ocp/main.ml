
module Cst = Parsexp.Cst

type sexp =
  | Atom of string
  | List of sexp list

type lib = {
    lib_kind : string;
    mutable lib_names : string list;
    mutable lib_public_name : string option;
    mutable lib_requires : string list;
    mutable lib_flags : string list;
    mutable lib_linkflags : string list;
    mutable lib_wrapped : bool;
    mutable lib_has_byte : bool;
    mutable lib_has_asm : bool;
    mutable lib_modules : string list;
  }

type action = {
    mutable action_args : string list;
    mutable action_stdout : string option;
    mutable action_chdir : string option;
  }

type rule = {
    mutable rule_targets : string list;
    mutable rule_deps : string list;
    mutable rule_actions : action list;
  }



let rec string_of_sexp =
  function | Atom s -> Printf.sprintf "%S" s
           | List list ->
              let list = List.map string_of_sexp list in
              Printf.sprintf "(%s)" (String.concat " " list)

let rec remove_comments = function
  | Cst.Sexp (Cst.Atom { atom }) -> Some (Atom atom)
  | Cst.Sexp (Cst.List { elements }) ->
     Some (List (remove_comments_list elements))
  | Cst.Comment _ -> None

and remove_comments_list elements =
  List.fold_right (fun ele acc ->
      match remove_comments ele with
      | None -> acc
      | Some ele -> ele :: acc)
                 elements []


let parse_library file lib = function
  | List [ Atom "name"; Atom name ] -> lib.lib_names <- [name]
  | List [ Atom "names"; List names ] ->
     lib.lib_names <- List.map (function
                                | Atom name -> name
                                | List _ -> assert false)
                               names
  | List [ Atom "public_name"; Atom name ] -> lib.lib_public_name <- Some name
  | List [ Atom "wrapped"; Atom "false" ] -> lib.lib_wrapped <- false
  | List [ Atom "modes"; List [ Atom "native" ] ] -> lib.lib_has_byte <- false
  | List [ Atom "modes"; List [ Atom "bytecode" ] ] -> lib.lib_has_asm <- false
  | List [ Atom "libraries"; List libraries ] ->
     lib.lib_requires <- lib.lib_requires @
                           List.map (function
                                     | List _ ->
                                        failwith "List in (libraries ...)"
                                     | Atom name -> name)
                                    libraries
  | List [ Atom "modules"; List modules ] ->
     lib.lib_modules <- lib.lib_modules @
                           List.map (function
                                     | List _ ->
                                        failwith "List in (modules ...)"
                                     | Atom name -> name)
                                    modules
  | List [ Atom "flags"; List flags ] ->
     List.iter (function
                | List _ ->
                   failwith "List in (flags ...)"
                | Atom ":standard" -> ()
                | Atom name ->
                   lib.lib_flags <- lib.lib_flags @ [name])
               flags
  | List [ Atom "library_flags"; List flags ] ->
     List.iter (function
                | List _ ->
                   failwith "List in (library_flags ...)"
                | Atom ":standard" -> ()
                | Atom name ->
                   lib.lib_linkflags <- lib.lib_linkflags @ [name])
               flags
  | List (Atom "install" :: _) -> () (* TODO *)
  | ele ->
     Printf.eprintf "%s: Discarding library element %s\n%!" file (string_of_sexp ele)

let rec parse_action file action = function
  | List (Atom "run":: args) ->
     List.iter (function
                | Atom arg ->
                   action.action_args <- action.action_args @ [ arg]
                | List _ -> assert false)
               args
  | List [Atom "with-stdout-to"; Atom stdout; sexp] ->
     parse_action file action sexp;
     action.action_stdout <- Some stdout
  | List [Atom "chdir"; Atom dir; sexp] ->
     parse_action file action sexp;
     action.action_chdir <- Some dir
  | ele ->
     Printf.eprintf
       "%s: Discarding action element %s\n%!" file (string_of_sexp ele)

let parse_rule b file r = function
  | List [ Atom "targets"; List targets ] ->
     List.iter (function
                | Atom target ->
                   r.rule_targets <- r.rule_targets @ [ target]
                | List _ -> assert false)
               targets
  | List [ Atom "deps"; List deps ] ->
     List.iter (function
                | Atom dep ->
                   r.rule_deps <- r.rule_deps @ [ dep ]
                | List _ -> assert false)
               deps
  | List [ Atom "action"; sexp ] ->
     let action = {
         action_args = [];
         action_stdout = None;
         action_chdir = None;
       } in
     parse_action file action sexp;
     r.rule_actions <- r.rule_actions @ [action]
  | ele ->
     Printf.eprintf
       "%s: Discarding rule element %s\n%!" file (string_of_sexp ele)


let print_library b file lib =
  Printf.bprintf b "(* from file %S *)\n\n%!" file;
  let subdir = Filename.dirname file in
  List.iter (fun lib_name ->
      Printf.bprintf b "begin\n";
      Printf.bprintf b "  ocaml.subdir = %S;\n" subdir;
      if lib.lib_wrapped then
        Printf.bprintf b "  ocaml.alias = %S;\n"  lib_name;
      let public_name =
        match lib.lib_public_name with
        | None -> lib_name
        | Some name -> name
      in
      Printf.bprintf b "  public_name = %S;\n" public_name;
      Printf.bprintf b"\n";

      if lib.lib_flags <> [] then begin
          Printf.bprintf b "  flags = [\n";
          List.iter (fun name ->
              Printf.bprintf b "     %S;\n" name
            ) lib.lib_flags;
          Printf.bprintf b "  ];\n";
          Printf.bprintf b "  ocaml.dep = flags;\n";
          Printf.bprintf b "  ocaml.bytecomp = flags;\n";
          Printf.bprintf b "  ocaml.asmcomp = flags;\n";
          Printf.bprintf b"\n";
        end;

      if not lib.lib_has_asm then
        Printf.bprintf b "  ocaml.has_asm = false;\n";
      if not lib.lib_has_byte then
        Printf.bprintf b "  ocaml.has_byte = false;\n";

      if lib.lib_linkflags <> [] then begin
          Printf.bprintf b "  linkflags = [\n";
          List.iter (fun name ->
              Printf.bprintf b "     %S;\n" name
            ) lib.lib_linkflags;
          Printf.bprintf b "  ];\n";
          Printf.bprintf b "  ocaml.asmlink = linkflags;\n";
          Printf.bprintf b "  ocaml.bytelink = linkflags;\n";
          Printf.bprintf b"\n";
        end;
      Printf.bprintf b "  ocaml.sort = true;\n";
      Printf.bprintf b "  ocaml.files = [\n";
      if lib.lib_modules = [] then
        Array.iter (fun name ->
            if Filename.check_suffix name ".ml" then
              Printf.bprintf b "     %S;\n" name
          ) (Sys.readdir subdir)
      else
        List.iter (fun name ->
            match name with
            | "" | ":standard" | "\\" -> ()
            | name ->
               let name = String.uncapitalize name in
               let name = name ^ ".ml" in
               Printf.bprintf b "     %S;\n" name
          ) lib.lib_modules;
      (*      data_encoding.ml *)
      Printf.bprintf b "  ];\n";
      Printf.bprintf b"\n";
      Printf.bprintf b "  ocaml.requires = [\n";
      List.iter (fun name ->
          Printf.bprintf b "     %S;\n" name
        ) lib.lib_requires;
      (* tezos-stdlib ocplib-json-typed ocplib-json-typed.bson *)
      Printf.bprintf b "  ];\n";
      Printf.bprintf b"\n";

      Printf.bprintf b "  OCaml.%s(public_name, ocaml);\n" lib.lib_kind;
      Printf.bprintf b "end\n\n";
    ) lib.lib_names

let parse_package b file lib_kind sexps =
  let lib = {
      lib_kind;
         lib_names = [];
         lib_public_name = None;
         lib_requires = [];
         lib_flags = [];
         lib_linkflags = [];
         lib_wrapped = true;
         lib_has_asm = true;
         lib_has_byte = true;
         lib_modules = [];
       } in
     List.iter (parse_library file lib) sexps;
     print_library b file lib

let print_rule b file r =
  Printf.bprintf b "build_rules = [\n";
  begin
    match r.rule_targets with
    | [] -> assert false
    | [r] -> Printf.bprintf b "    %S, {\n" r
    | targets ->
       Printf.bprintf b "  [\n";
       List.iter (fun target ->
           Printf.bprintf b "    %S;\n" target
         ) targets;
       Printf.bprintf b "  ], {\n";
       Printf.bprintf b "    uniq_id = %S;\n" (List.hd targets);
  end;
  Printf.bprintf b "       sources = [\n";
       List.iter (fun dep ->
           Printf.bprintf b "         %S;\n" dep
         ) r.rule_deps;
       Printf.bprintf b "       ];\n";
  Printf.bprintf b "     }\n";
  Printf.bprintf b "]\n\n";
  ()

let parse_top_sexp b file = function
  | List [
      Atom "jbuild_version";
      Atom "1";
    ] ->
     ()
  | List [ Atom "*";
           Atom "-*-";
           Atom "tuareg";
           Atom "-*-";
           Atom "*" ] ->
     raise Exit
  | List [ Atom "rule";
           List sexps;
         ] ->
     let r = {
         rule_targets = [];
         rule_deps = [];
         rule_actions = [];
       } in
     List.iter (parse_rule b file r) sexps;
     print_rule b file r
  | List [ Atom "library";
           List sexps;
         ] -> parse_package b file "library" sexps
  | List [ Atom ( "executable" | "executables");
           List sexps;
         ] -> parse_package b file "program" sexps
  | List (Atom "alias" :: _ ) -> () (* do not create aliases for now *)
  | List (Atom "install" :: _) -> () (* TODO *)
  | top ->
     Printf.eprintf "%s: Discarding %s\n%!" file (string_of_sexp top)

let translate_file file =
  try
    let b = Buffer.create 1000 in
    let input = FileString.read_file file in
    let sexps = Parsexp.Many_cst.parse_string_exn input in
    List.iter (parse_top_sexp b file)
              (remove_comments_list sexps);
    let s = Buffer.contents b in
    Printf.printf "%s\n%!" s;
  with Exit ->
    Printf.eprintf "File %S: in OCaml\n%!" file;
    ()

let () =
  Arg.parse [] translate_file "jbuild2ocp FILES"
