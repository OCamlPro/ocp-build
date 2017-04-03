


val output_graph_report : bool ref

val report : BuildEngineTypes.build_context -> unit






val output_replay_script : bool ref

val cmd_mkdir : string -> unit
val cmd_rmdir : string -> unit
val cmd_rename : string -> string -> unit
val cmd_copy : string -> string -> unit
val cmd_exec : string list ->
  string option ->
  string option ->
  string option ->
  string option ->
  unit
val cmd_file_from_content : string -> string -> unit

val flush : unit -> unit
