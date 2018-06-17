open StringCompat

let save = ref false
let translate = ref true
let dot_output = ref None
let verbose = ref 0

let translate_file file =
  try
    if !translate && not !save then
      Printf.printf "File %s:\n" file
    else
    if !verbose > 0 then
      Printf.eprintf "File %s:\n" file;

    let eles =
      try
        let sexps = DuneParser.read file in
        DuneInterp.parse file sexps
      with Exit ->
        let file = file ^ ".static" in
        if Sys.file_exists file then
          let sexps = DuneParser.read file in
          DuneInterp.parse file sexps
        else begin
          Printf.eprintf "File %S: in OCaml\n%!" file;
          Printf.eprintf "   Discarded. No .static file found.\n%!";
          raise Exit
        end
    in

    if !translate then
      let content = Dune2ocp.to_string file eles in
      if !save then
        let dirname = Filename.dirname file in
        let ocp_file = Filename.concat dirname "build.jbuild2ocp" in
        FileString.write_file ocp_file content;
        Printf.eprintf "   %S generated\n%!" ocp_file
      else
        Printf.printf "%s\n%!" content
    else
      Dune2dot.register file eles
  with Exit ->
    ()

let () =

  let directory = ref None in
  Arg.parse [
    "-v", Arg.Unit (fun () -> incr verbose), " Increase verbosity";
    "--save", Arg.Set save, " Save ocp files";
    "--dir", Arg.String (fun s -> directory := Some s), "DIR In directory";
    "--dot", Arg.String (fun s ->
        translate := false;
        dot_output := Some s), "DIR Output .dot files to DIR";
  ] translate_file "jbuild2ocp FILES";

  begin
    match !directory with
    | None -> ()
    | Some dir ->
      let select = FileString.select ~deep:true ~glob:"jbuild" () in
      FileString.iter_dir ~select
        (fun _basename _path file ->
           translate_file file
        ) dir
  end;

  match !dot_output with
  | None -> ()
  | Some dot_output ->
    Dune2dot.save dot_output;
    Printf.eprintf "%S generated\n%!" dot_output
