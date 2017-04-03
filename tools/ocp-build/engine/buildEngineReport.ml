
open BuildEngineTypes

type vertex = {
  vertex_file: BuildEngineTypes.build_file;

  mutable vertex_to : vertex IntMap.t;
  mutable vertex_from : vertex IntMap.t;
}

let output_graph_report = ref false
let output_replay_script = ref false

let replay_oc = ref None
let oc () =
    match !replay_oc with
    | Some oc -> oc
    | None ->
      let report_dir = "_obuild" in
      BuildMisc.safe_mkdir report_dir;
      let o = open_out "_obuild/_reports/build-replay.sh" in
      replay_oc := Some o;
      o

let report b =
  begin match !replay_oc with
  | Some oc -> close_out oc; | None -> ()
  end;
  if !output_graph_report then
  let rules_done = ref IntSet.empty in
  let graph = ref IntMap.empty in

  let rec get_vertex file =
    try
      IntMap.find file.file_id !graph
    with Not_found ->
      let v = {
        vertex_file = file;
        vertex_to = IntMap.empty;
        vertex_from = IntMap.empty;
      } in
      graph := IntMap.add file.file_id v !graph;
      v

  and add_rule rule =
    if not (IntSet.mem rule.rule_id !rules_done) then begin
      rules_done := IntSet.add rule.rule_id !rules_done;

      IntMap.iter (fun target_id target_file ->

        let target_vertex = get_vertex target_file in
        IntMap.iter (fun source_id source_file ->
          let source_vertex = get_vertex source_file in

          target_vertex.vertex_from <- IntMap.add
            source_id source_vertex target_vertex.vertex_from;

          source_vertex.vertex_to <- IntMap.add
            target_id target_vertex source_vertex.vertex_to;


        ) rule.rule_sources

      ) rule.rule_targets

    end
  in


  Hashtbl.iter (fun file_id file ->
    let (_ : vertex) = get_vertex file in
    ()
  ) b.build_files;


  Hashtbl.iter (fun rule_id rule ->
    add_rule rule
  ) b.build_rules;
  ()

let printf fmt = Printf.kprintf (fun s ->
  let oc = oc () in
  output_string oc s;
  output_char oc '\n';
) fmt


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
let rec cmd_exec args move_to_dir stdin_pipe stdout_pipe stderr_pipe =
  if !output_replay_script then
    let cmd = String.concat " "
      (List.map (fun s -> Printf.sprintf "%S" s) args) in
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
