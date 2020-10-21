
open DuneParser.TYPES

module TYPES = struct

  type library = {
    lib_kind : string;
    mutable lib_names : string list;
    mutable lib_public_names : string list;
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

  type element =
    | Rule of rule
    | Library of library

end

open TYPES

let parse_library file lib = function
  | List [ Atom "name"; Atom name ] -> lib.lib_names <- [name]
  | List [ Atom "names"; List names ] ->
     lib.lib_names <- List.map (function
                                | Atom name -> name
                                | List _ -> assert false)
                               names
  | List [ Atom "public_name"; Atom name ] -> lib.lib_public_names <- [name]
  | List [ Atom "public_names"; List names ] ->
     lib.lib_public_names <- List.map (function
                                | Atom name -> name
                                | List _ -> assert false)
                               names
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
    Printf.eprintf "%s: Discarding library element %s\n%!" file
      (DuneParser.string_of_sexp ele)

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
       "%s: Discarding action element %s\n%!" file
       (DuneParser.string_of_sexp ele)

let parse_rule file r = function
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
                | List _ ->
                  Printf.eprintf "   Discarding dynamic deps\n%!")
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
       "%s: Discarding rule element %s\n%!" file
       (DuneParser.string_of_sexp ele)

let parse_package file lib_kind sexps =
  let lib = {
      lib_kind;
         lib_names = [];
         lib_public_names = [];
         lib_requires = [];
         lib_flags = [];
         lib_linkflags = [];
         lib_wrapped = true;
         lib_has_asm = true;
         lib_has_byte = true;
         lib_modules = [];
       } in
     List.iter (parse_library file lib) sexps;
     lib

let parse file sexps =
  let rec iter =
      function
      | [] -> []
      | (
        List [
          Atom "jbuild_version";
          Atom "1";
        ]
      | List (Atom "alias" :: _ ) (* do not create aliases for now *)
      | List (Atom "install" :: _) (* TODO *)
      )
        :: rem ->
        iter rem
      | List [ Atom "*";
               Atom "-*-";
               Atom "tuareg";
               Atom "-*-";
               Atom "*" ] :: _ ->
        raise Exit
      | List [ Atom "rule";
               List sexps;
             ] :: rem ->
        let r = {
          rule_targets = [];
          rule_deps = [];
          rule_actions = [];
        } in
        List.iter (parse_rule file r) sexps;
        (Rule r) :: iter rem
      | List [ Atom "library";
               List sexps;
             ] :: rem ->
        (Library (parse_package file "library" sexps)) :: iter rem
      | List [ Atom ( "executable" | "executables");
               List sexps;
             ] :: rem ->
        (Library (parse_package file "program" sexps)) :: iter rem
      | top :: rem ->
        Printf.eprintf "%s: Discarding %s\n%!" file
          (DuneParser.string_of_sexp top);
        iter rem
  in
  iter sexps
