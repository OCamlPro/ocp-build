(* TODO:
   * try to define variables for prefixes, to share as much path in one variable
*)

open OcpCompat (* for StringMap *)
open BuildEngineTypes


let report_dir = "_obuild/_reports"
let replay_script = Filename.concat report_dir "build-replay.sh"


let output_graph_report = ref false
let output_replay_script = ref false

let counter = ref 0
let strings = ref StringMap.empty
let var_of_string s =
  try
    StringMap.find s !strings
  with Not_found ->
    incr counter;
    let var = Printf.sprintf "DIR%d" !counter in
    strings := StringMap.add s var !strings;
    var

let var_of_string s =
  let basename = Filename.basename s in
  let dirname = Filename.dirname s in
  let var = "$" ^ var_of_string dirname in
  Filename.concat var basename

let replay_b = ref None
let buf () =
    match !replay_b with
    | Some b -> b
    | None ->
      let b = Buffer.create 1111 in
      replay_b := Some b;
      b

let printf fmt = Printf.kprintf (fun s ->
  let b = buf () in
  Buffer.add_string b s;
  Buffer.add_char b '\n';
) fmt


let cmd_rmdir dir  =
  if !output_replay_script then begin
    printf "echo rm -rf %s" dir;
    printf "rm -rf %s || exit 2" dir
  end

let cmd_mkdir dir  =
  if !output_replay_script then begin
    printf "echo mkdir -p %s" dir;
    printf "mkdir -p %s || exit 2" dir
  end
let cmd_rename f1 f2 =
  if !output_replay_script then begin
    printf "echo mv %s %s" f1 f2;
    printf "mv %s %s || exit 2" f1 f2
  end
let cmd_copy f1 f2 =
  if !output_replay_script then begin
    printf "echo cp %s %s" f1 f2;
    printf "cp -f %s %s || exit 2" f1 f2
  end
let cmd_exec args move_to_dir stdin_pipe stdout_pipe stderr_pipe =
  if !output_replay_script then
    let cmd = String.concat " "
      (List.map (fun s ->
        let s =
          if String.length s <> 0 && s.[0] = '/' then var_of_string s
          else s
        in
        Printf.sprintf "%S" s) args) in
    let cmd = match move_to_dir with None -> cmd
      | Some dir -> Printf.sprintf "(cd %s; %s)" dir cmd
    in
    let cmd = match stdin_pipe with None -> cmd
      | Some file -> Printf.sprintf "%s < %s" cmd file
    in
    let cmd = match stdout_pipe, stderr_pipe with
      | None, None -> cmd
      | Some file, None -> Printf.sprintf "%s > %s" cmd file
      | None, Some file -> Printf.sprintf "%s 2> %s" cmd file
      | Some stdout, Some stderr ->
        if stdout <> stderr then
          Printf.sprintf "%s 2> %s > %s" cmd stderr stdout
        else
          Printf.sprintf "%s > %s 2&>1" cmd stdout
    in
    printf "echo '%s'" cmd;
    printf "%s || exit 2" cmd
let cmd_file_from_content file content =
  if !output_replay_script then begin
    printf "echo 'cat <<EOF... > %s'" file;
    printf "cat > %s <<'EndOfInputOcpBuild'\n%s\nEndOfInputOcpBuild" file content
  end

let flush () =
  match !replay_b with
  | Some b ->
    Printf.eprintf "Generate replay script: %s\n%!" replay_script;
    BuildMisc.safe_mkdir report_dir;
    let oc = open_out replay_script in
    Printf.fprintf oc "#!/bin/sh\n";
    StringMap.iter (fun s var ->
      Printf.fprintf oc "%s=%s\n" var s;
      Printf.fprintf oc "export %S\n" var;
    ) !strings;
    output_string oc (Buffer.contents b);
    close_out oc
  | None -> ()










open Ocamldot.TYPES

type vertex = {
  vertex_id : int;
  vertex_name: string;
  vertex_package : BuildEngineTypes.build_package;
  mutable vertex_to_all : vertex IntMap.t;
  mutable vertex_from_all : vertex IntMap.t;
  mutable vertex_to : vertex IntMap.t;
  mutable vertex_from : vertex IntMap.t;
}

let report b =
  if !output_graph_report then
    let rules_done = ref IntSet.empty in
    let graph = ref IntMap.empty in
    let vertices = Hashtbl.create 1111 in
    let packages = ref IntMap.empty in
    let nvertices = ref 0 in
    let rec get_vertex file =
      try
        IntMap.find file.file_id !graph
      with Not_found ->
        let basename = file.file_basename in
        let basename =

          try
            let pos = String.index basename '.' in
            String.sub basename 0 pos
          with
          | _ ->  basename
        in
        let package_uid = file.file_package.package_uid in
        let key = (package_uid, basename) in
        let v =
          try
            Hashtbl.find vertices key
          with Not_found ->

            let v = {
              vertex_id = !nvertices;
              vertex_name = basename;
              vertex_package = file.file_package;
              vertex_to_all = IntMap.empty;
              vertex_from_all = IntMap.empty;
              vertex_to = IntMap.empty;
              vertex_from = IntMap.empty;
            } in
            incr nvertices;
            Hashtbl.add vertices key v;

            let (_p,vertices_in_package) =
              try
                IntMap.find file.file_package.package_uid !packages
              with Not_found ->
                let r = ref [] in
                packages := IntMap.add file.file_package.package_uid
                  (file.file_package,r) !packages;
                (file.file_package, r)
            in
            vertices_in_package := v :: !vertices_in_package;

            v
        in
        graph := IntMap.add file.file_id v !graph;
        v

    and add_rule rule =
      if not (IntSet.mem rule.rule_id !rules_done) then begin
        rules_done := IntSet.add rule.rule_id !rules_done;

        IntMap.iter (fun _target_id target_file ->

          let target_vertex = get_vertex target_file in
          IntMap.iter (fun _source_id source_file ->
            let source_vertex = get_vertex source_file in

            if source_vertex != target_vertex then begin
              target_vertex.vertex_from <- IntMap.add
                source_vertex.vertex_id source_vertex target_vertex.vertex_from;

              source_vertex.vertex_to <- IntMap.add
                target_vertex.vertex_id target_vertex source_vertex.vertex_to;
            end
          ) rule.rule_sources
        ) rule.rule_targets
      end
    in

    Hashtbl.iter (fun _file_id file ->
      let (_ : vertex) = get_vertex file in ()
    ) b.build_files;
    Hashtbl.iter (fun _rule_id rule -> add_rule rule) b.build_rules;


    let rec iter_from v =
      if IntMap.is_empty v.vertex_from_all then begin
        v.vertex_from_all <- v.vertex_from;
        IntMap.iter (fun _ v2 ->
          iter_from v2;
          IntMap.iter (fun _ v3 ->
            v.vertex_from_all <- IntMap.add v3.vertex_id v3 v.vertex_from_all
          ) v2.vertex_from_all
        ) v.vertex_from;
        IntMap.iter (fun _ v2 ->
          if v2.vertex_package == v.vertex_package then
            IntMap.iter (fun _ v3 ->
              if v3.vertex_package == v.vertex_package &&
                IntMap.mem v2.vertex_id v3.vertex_from_all then
                v.vertex_from <- IntMap.remove v2.vertex_id v.vertex_from
          ) v.vertex_from
        ) v.vertex_from
      end
    in


    Hashtbl.iter (fun _ v ->
      iter_from v;
    ) vertices;

    let rec iter_to v =
      if IntMap.is_empty v.vertex_to_all then begin
        v.vertex_to_all <- v.vertex_to;
        IntMap.iter (fun _ v2 ->
          iter_to v2;
          IntMap.iter (fun _ v3 ->
            v.vertex_to_all <- IntMap.add v3.vertex_id v3 v.vertex_to_all
          ) v2.vertex_to_all
        ) v.vertex_to;
        IntMap.iter (fun _ v2 ->
          if v2.vertex_package == v.vertex_package then
            IntMap.iter (fun _ v3 ->
              if v3.vertex_package == v.vertex_package &&
                IntMap.mem v2.vertex_id v3.vertex_to_all then
              v.vertex_to <- IntMap.remove v2.vertex_id v.vertex_to
          ) v.vertex_to
        ) v.vertex_to
      end
    in


    Hashtbl.iter (fun _ v ->
      iter_to v) vertices;

    BuildMisc.safe_mkdir report_dir;

    IntMap.iter (fun _package_uid (p, vertices_in_package) ->

      let create name get_edges =

        let t = Ocamldot.create p.package_package
          [ GraphSize (11.7, 8.3); Ratio RatioFill ]
        in
        let nodes = ref IntMap.empty in

        let get_node v =
          try
            IntMap.find v.vertex_id !nodes
          with Not_found ->
            let node = Ocamldot.node t v.vertex_name
              [NodeColor "blue"; NodeShape Box]
            in
            nodes := IntMap.add v.vertex_id node !nodes;
            node
        in

        List.iter (fun v ->
          let _node = get_node v in
          ()
        ) !vertices_in_package;

        let get_node v =
          try
            IntMap.find v.vertex_id !nodes
          with Not_found ->
            let node = Ocamldot.node t
              (Printf.sprintf "%s:%s"
                 v.vertex_package.package_package
                 v.vertex_name) [] in
            nodes := IntMap.add v.vertex_id node !nodes;
            node
        in

        List.iter (fun v ->
          let node = get_node v in
          IntMap.iter (fun _ v2 ->
            let node2 = get_node v2 in
            Ocamldot.add_edge node2 node [];
          ) (get_edges v)
        ) !vertices_in_package;


        Ocamldot.save t
          (Printf.sprintf "_obuild/_reports/package_%s_%s.dot"
             name p.package_package)
      in
      create "from" (fun v -> v.vertex_from);
      create "to" (fun v -> v.vertex_to);
    ) !packages;


    ()
