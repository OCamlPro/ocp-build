

open BuildEngineTypes

type vertex = {
  vertex_file: BuildEngineTypes.build_file;

  mutable vertex_to : vertex IntMap.t;
  mutable vertex_from : vertex IntMap.t;
}

val report : BuildEngineTypes.build_context -> unit
val cmd_mkdir : string -> unit
val cmd_rename : string -> string -> unit
val cmd_copy : string -> string -> unit
val cmd_exec : string list ->
  string option ->
  string option ->
  string option ->
  string option ->
  unit

val cmd_file_from_content : string -> string -> unit
val output_replay_script : bool ref
val output_graph_report : bool ref
