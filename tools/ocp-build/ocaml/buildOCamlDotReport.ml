
open OcpCompat
open BuildOCamlTypes
open BuildValue.TYPES
open BuildOCPTypes
open Ocamldot.TYPES

let report_dir = "_obuild/_reports"
let report_dot = "project.dot"
let report_pdf = "project.pdf"

let report_dot = Filename.concat report_dir report_dot
let report_pdf = Filename.concat report_dir report_pdf

type node = {
  node_name : string;
  mutable node_requires : node StringMap.t;
  mutable node_requires_all : node StringMap.t;
  mutable node_users : node StringMap.t;
  mutable node_tool : bool;
  mutable node_buildable : bool;
  mutable node_installed : bool;
  mutable node_node : Ocamldot.TYPES.node option;
  mutable node_traversed : bool;
  mutable node_exists : bool;
}


let report packages =

  let nodes = ref StringMap.empty in
  let add_node node_name =
    try
      StringMap.find node_name !nodes
    with Not_found ->
      let n = {
        node_name;
        node_requires = StringMap.empty;
        node_requires_all = StringMap.empty;
        node_users = StringMap.empty;
        node_tool = false;
        node_buildable = false;
        node_installed = false;
        node_node = None;
        node_exists = false;
        node_traversed = false;
      } in
      nodes := StringMap.add node_name n !nodes;
      n
  in

  StringMap.iter (fun name opk ->
    let n = add_node name in
    let requires =
      try BuildValue.prop_list (BuildValue.get opk.opk_options "requires")
      with Var_not_found _ -> []
    in
    List.iter (fun (dep_name, _) ->
      let n2 = add_node dep_name in
      n.node_requires <- StringMap.add dep_name n2 n.node_requires;
      n2.node_users <- StringMap.add name n n.node_users
    ) requires;
    n.node_installed <- opk.opk_installed;
    n.node_buildable <- opk.opk_installed;
    n.node_exists <- true;
    match opk.opk_kind with
    | ProgramPackage -> n.node_tool <- true
    | _ -> ()
  ) packages;

  let rec iter n =
    if not n.node_traversed then begin
      n.node_traversed <- true;

      StringMap.iter (fun _ n2 -> iter n2) n.node_requires;
      n.node_buildable <- n.node_exists;
      StringMap.iter (fun name2 n2 ->
        if not n2.node_buildable then n.node_buildable <- false;
        n.node_requires_all <- StringMap.add name2 n2 n.node_requires_all;
        StringMap.iter (fun name3 n3 ->
          n.node_requires_all <- StringMap.add name3 n3 n.node_requires_all
        ) n2.node_requires_all;
      ) n.node_requires;

      StringMap.iter (fun name2 _n2 ->
        StringMap.iter (fun _name3 n3 ->
          if StringMap.mem name2 n3.node_requires_all then
            n.node_requires <- StringMap.remove name2 n.node_requires
        ) n.node_requires
      ) n.node_requires;

    end
  in
  StringMap.iter (fun _ n ->
    if not n.node_installed then iter n
  ) !nodes;

  let t = Ocamldot.create "Project"
    [ GraphSize (11.7, 8.3);
      Ratio RatioFill
    ] in

  StringMap.iter (fun _ n ->
    if n.node_traversed then
      let attrs = [] in
      let attrs = if n.node_tool then (NodeShape Box) :: attrs else attrs in
      let attrs = if n.node_installed then (NodeColor "blue") :: attrs else attrs in
      let attrs = if n.node_buildable then attrs else begin
        Printf.eprintf "%s is not buildable\n%!" n.node_name;
        (NodeColor "red"):: attrs
      end in
      let attrs = if n.node_exists then attrs else (NodeShape Diamond) :: attrs in
      let node = Ocamldot.node t n.node_name attrs in
      n.node_node <- Some node
  ) !nodes;

  StringMap.iter (fun _ n ->
    match n.node_node with
    | None -> ()
    | Some node ->
      StringMap.iter (fun _ n2 ->
        match n2.node_node with
        | None -> assert false
        | Some node2 ->
          let _edge = Ocamldot.edge node node2 [] in
          ()
      ) n.node_requires
  ) !nodes;

(*
  let env = ref StringMap.empty in

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
*)

  BuildMisc.safe_mkdir report_dir;
  Ocamldot.save t report_dot;
  let cmd = Printf.sprintf "dot -Tpdf %s -o %s" report_dot report_pdf in
  Printf.eprintf "Calling %s\n%!" cmd;
  let _retcode = Sys.command cmd in
  ()
