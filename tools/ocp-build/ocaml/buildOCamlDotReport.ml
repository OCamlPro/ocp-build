
open StringCompat
open BuildOCamlTypes
open BuildValue.TYPES
open BuildOCPTypes
open Ocamldot.TYPES

let report_dir = "_obuild/_html"
let report_dot = "project.dot"
let report_pdf = "project.pdf"

let report_dot = Filename.concat report_dir report_dot
let report_pdf = Filename.concat report_dir report_pdf

let report packages =

  let t = Ocamldot.create "Project" [] in

  let env = ref StringMap.empty in
  let nodes = ref StringMap.empty in

  StringMap.iter (fun name opk ->
    if opk.opk_installed then
      env := StringMap.add name opk !env
    else
    let attrs = match opk.opk_kind with
      | ProgramPackage -> [ NodeShape Box ]
      | _ -> [ ]
    in
    let n = Ocamldot.node t name attrs in
    nodes := StringMap.add name (n,Some opk) !nodes
  ) packages;

  StringMap.iter (fun name (node,opk) ->
    match opk with
    | None -> ()
    | Some opk ->
      let requires =
        try BuildValue.prop_list (BuildValue.get opk.opk_options "requires")
        with Var_not_found _ -> []
      in

      List.iter (fun (dep_name, _) ->
        let node2 =
          try
            let (node2,_) = StringMap.find dep_name !nodes in
            node2
          with Not_found ->
            let attrs =
              try
                let opk2 = StringMap.find dep_name !env in
                [NodeColor "blue"]
              with Not_found ->
                [NodeColor "red"]
            in
            let node2 = Ocamldot.node t dep_name attrs in
            nodes := StringMap.add dep_name (node2, None) !nodes;
            node2
        in
        let _edge = Ocamldot.edge node node2 [] in
        ()
      ) requires

  ) !nodes;

  BuildMisc.safe_mkdir report_dir;
  Ocamldot.save t report_dot;
  let cmd = Printf.sprintf "dot -Tpdf %s -o %s" report_dot report_pdf in
  Printf.eprintf "Calling %s\n%!" cmd;
  let _retcode = Sys.command cmd in
  ()
