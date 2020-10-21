open StringCompat
open DuneInterp.TYPES

let rec filter_ocamldep_flags flags =
  match flags with
    [] -> []
  | ("-safe-string" | "-nopervasives" | "-linkall"| "-opaque") :: flags ->
     filter_ocamldep_flags flags
  | ("-w") :: _ :: flags ->
     filter_ocamldep_flags flags
  | x :: flags ->
     x :: filter_ocamldep_flags flags

let print_library b file lib =
  Printf.bprintf b "(* from file %S *)\n\n%!" file;
  let subdir = Filename.dirname file in
  List.iteri (fun i lib_name ->
      Printf.bprintf b "begin\n";
      Printf.bprintf b "  ocaml.subdir = %S;\n" subdir;
      if lib.lib_wrapped then
        Printf.bprintf b "  ocaml.alias = %S;\n"  lib_name;
      let public_name =
        match lib.lib_public_names with
        | [] -> lib_name
        | names -> List.nth names i
      in
      Printf.bprintf b "  public_name = %S;\n" public_name;
      Printf.bprintf b"\n";

      if lib.lib_flags <> [] then begin
          Printf.bprintf b "  flags = [\n";
          List.iter (fun name ->
              Printf.bprintf b "     %S;\n" name
            ) lib.lib_flags;
          Printf.bprintf b "  ];\n";
          Printf.bprintf b "  ocaml.bytecomp = flags;\n";
          Printf.bprintf b "  ocaml.asmcomp = flags;\n";
          let dep_flags = filter_ocamldep_flags lib.lib_flags in
          if dep_flags <> [] then begin
              Printf.bprintf b "  ocaml.dep = [\n";
              List.iter (fun name ->
                  Printf.bprintf b "     %S;\n" name
                ) dep_flags;
              Printf.bprintf b "  ];\n";
            end;

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
        begin
          let rec iter set modules =
            match modules with
              [] -> set
            | "\\" :: modules ->
               iter_except set modules
            | ":standard" :: modules ->
               let set = ref set in
               Array.iter (fun name ->
                   if Filename.check_suffix name ".ml" then
                     let modname = Filename.chop_extension name in
                     let modname = String.capitalize modname in
                     set := StringSet.add modname !set
                 ) (Sys.readdir subdir);
               iter !set modules
            | name :: modules ->
               iter (StringSet.add name set) modules
          and iter_except set modules =
            match modules with
              [] -> set
            | "\\" :: modules ->
               iter set modules
            | name :: modules ->
               iter (StringSet.remove name set) modules
          in
          let set = iter StringSet.empty lib.lib_modules in
          StringSet.iter (fun name ->
              let name = String.uncapitalize name in
              let name = name ^ ".ml" in
              Printf.bprintf b "     %S;\n" name
            ) set
        end;
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

let print_rule b file r =
  let subdir = Filename.dirname file in
  let subdir =
    if subdir = "" then function file -> file
    else function file -> Filename.concat subdir file
  in
  Printf.bprintf b "build_rules = [\n";
  begin
    match r.rule_targets with
    | [] -> assert false
    | [r] -> Printf.bprintf b "    %S, {\n" (subdir r)
    | targets ->
       Printf.bprintf b "  [\n";
       List.iter (fun target ->
           Printf.bprintf b "    %S;\n" (subdir target)
         ) targets;
       Printf.bprintf b "  ], {\n";
       Printf.bprintf b "    uniq_id = %S;\n" (subdir (List.hd targets));
  end;
  Printf.bprintf b "       sources = [\n";
  List.iter (fun dep ->
      Printf.bprintf b "         %S;\n" (subdir dep)
    ) r.rule_deps;
  Printf.bprintf b "       ];\n";
  Printf.bprintf b "       commands = [\n";
  List.iter (fun action ->
      Printf.bprintf b "            OCaml.system([\n";
      List.iter (fun arg ->
          Printf.bprintf b "              %S;\n" arg
        ) action.action_args;
      Printf.bprintf b "               ]\n";
      begin
        match action.action_stdout with
        | None -> ()
        | Some file ->
           Printf.bprintf b "           ,{ stdout = %S }\n" file
      end;
                Printf.bprintf b "        );\n";
    ) r.rule_actions;
  Printf.bprintf b "       ];\n";
  Printf.bprintf b "     }\n";
  Printf.bprintf b "];\n\n";
  ()


let to_string file eles =
  let b = Buffer.create 1000 in
  List.iter (function
      | Library lib -> print_library b file lib
      | Rule r -> print_rule b file r
    ) eles;
  Buffer.contents b
