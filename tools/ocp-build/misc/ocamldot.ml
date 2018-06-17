(**************************************************************************)
(*                                                                        *)
(*   Typerex Tools                                                        *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU General Public License version 3 described in the file       *)
(*   LICENSE.                                                             *)
(*                                                                        *)
(**************************************************************************)

(*
This file generate a .dot file to be processed by "dot" to generate a
.ps file.
*)

module TYPES = struct

type graph = {
    graph_name : string;
    mutable graph_nodes : node list;
    mutable graph_edges : edge list;
    mutable node_counter : int;
    graph_attributes : graph_attributes list;
  }

and node = {
    mutable node_name : string;
    node_id : int;
    node_graph : graph;
    node_attributes : node_attributes list;
  }

and edge = {
    edge_from : node;
    edge_to : node;
    edge_attributes : edge_attributes list;
  }

and graph_attributes =
  GraphSize of float * float
| Ratio of graph_ratio

and graph_ratio =
  RatioFill

and node_attributes =
  NodeColor of string |
  NodeFontColor of string |
  NodeFontName of string |
  NodeShape of shape |
  NodeHeight of float |
  NodeWidth of float |
  NodeStyle of goptions

and edge_attributes =
  EdgeDirection of direction |
  EdgeLabel of string |
  EdgeStyle of goptions |
  EdgeWeight of int

and direction =
  Forward |
  Backward |
  Bothdir |
  Nodir

and goptions =
  Bold |
  Dotted |
  Filled

and shape =
  Ellipse |
  Box |
  Circle |
  DoubleCircle |
  Diamond |
  PlainText |
  Record |
  Polygon of int * (polygon_options list) |
  Epsf of string

and polygon_options =
  Skew of float
| Distortion of float

end

open TYPES

let create name graph_attributes = {
    graph_name = name;
    node_counter = 0;
    graph_nodes = [];
    graph_edges = [];
    graph_attributes = graph_attributes;
  }

let node graph name node_attributes =
  let node = {
      node_name = name;
      node_id = graph.node_counter;
      node_graph = graph;
      node_attributes = node_attributes;
    } in
  graph.node_counter <- graph.node_counter + 1;
  graph.graph_nodes <- node :: graph.graph_nodes;
  node

let edge node1 node2 edge_attributes =
  let graph = node1.node_graph in
  if not (graph == node2.node_graph) then
    failwith "Ocamldot.edge: nodes in different graphs";
  let edge = {
      edge_from = node1;
      edge_to = node2;
      edge_attributes = edge_attributes;
    }
  in
  graph.graph_edges <- edge :: graph.graph_edges;
  edge

let add_edge node1 node2 list = ignore (edge node1 node2 list)

let add_edges node1 nodes list =
  List.iter (fun node2 -> add_edge node1 node2 list) nodes

let add_path nodes list =
  match nodes with
    [] -> ()
  | node1 :: nodes ->
      ignore (List.fold_right (fun node1 node2 ->
            add_edge node1 node2 list; node2
        ) nodes node1)

let rename_node node name = node.node_name <- name

open Printf

let to_string graph =
  let b = Buffer.create 1000 in

  let graph_attribute attr =
    match attr with
      GraphSize (f1,f2) -> Printf.bprintf b "  size=\"%f,%f!\";\n" f1 f2
    | Ratio ratio ->
      Printf.bprintf b "  ratio=\"%s\";\n" (match ratio with
        RatioFill -> "fill")

  and edge_attribute attr =
    match attr with
      EdgeDirection direction ->
        Printf.bprintf b " dir=%s" (match direction with
            Forward -> "forward"
          | Backward -> "backward"
          | Bothdir -> "both"
          | Nodir -> "none")
    | EdgeLabel label ->
        Printf.bprintf b " label=\"%s\"" label
    | EdgeStyle option ->
        Printf.bprintf b "style=%s" (match option with
             Bold -> "bold"
          | Dotted -> "dotted"
          | Filled -> "filled"
        )
    | EdgeWeight weight ->
        Printf.bprintf b "weight=%d" weight

  and node_attribute attr =
    match attr with
      NodeColor color ->
        Printf.bprintf b ", color=\"%s\"" color
    | NodeFontColor color ->
        Printf.bprintf b ", fontcolor=\"%s\"" color
    | NodeFontName font ->
        Printf.bprintf b ", fontname=\"%s\"" font
    | NodeShape shape ->
        Printf.bprintf b ", shape=%s" (match shape with
            Ellipse -> "ellipse"
          | Box -> "box"
          | Circle -> "circle"
          | DoubleCircle -> "doublecircle"
          | Diamond -> "diamond"
          | PlainText -> "plaintext"
          | Record -> "record"
          | Polygon (sides, options) ->
              sprintf "polygon, sides=%d%s" sides
                (List.fold_left (fun s attr ->
                  match attr with
                    Skew f -> sprintf ",skew=%f%s" f s
                  | Distortion f -> sprintf ",distortion=%f%s" f s
                ) "" options)
          | Epsf filename -> sprintf "epsf, shapefile=\"%s\"" filename
        )
    | NodeHeight height ->
        Printf.bprintf b ", height=%f" height
    | NodeWidth width ->
        Printf.bprintf b ", width=%f" width
    | NodeStyle option ->
        Printf.bprintf b ", style=%s" (match option with
            Bold -> "bold"
          | Dotted -> "dotted"
          | Filled -> "filled"
        )

  in
  Printf.bprintf b "digraph %S {\n" graph.graph_name;
  List.iter graph_attribute graph.graph_attributes;
  List.iter (fun node ->
      Printf.bprintf b "  node%d [ label=\"%s\"" node.node_id node.node_name;
      List.iter node_attribute node.node_attributes;
      Printf.bprintf b " ];\n";
      ) graph.graph_nodes;
  List.iter (fun edge ->
      Printf.bprintf b "  node%d -> node%d ["
        edge.edge_from.node_id edge.edge_to.node_id;
      (match edge.edge_attributes with
          [] -> ()
        | attr :: tail ->
            edge_attribute attr;
            List.iter (fun attr ->
                Printf.bprintf b ", ";
                edge_attribute attr) tail);
      Printf.bprintf b " ];\n";
  ) graph.graph_edges;
  Printf.bprintf b "}\n";
  Buffer.contents b

let save graph filename =
  let oc = open_out filename in
  output_string oc (to_string graph);
  close_out oc

let view g =
  let filename = Filename.temp_file "dotfile" ".dot" in
  try
    save g filename;
    let e = Sys.command (Printf.sprintf "dot -Tps < %s > %s.ps" filename filename) in
    if e <> 0 then failwith "Ocamldot: error while dot was processing file";
    let _ = Sys.command (Printf.sprintf "gv %s.ps" filename) in
    Sys.remove filename;
    Sys.remove (filename^".ps");
    ()
  with e ->
      Sys.remove filename;
      Sys.remove (filename^".ps");
      raise e

let dot2ps file_dot file_ps =
  let e = Sys.command (
      Printf.sprintf "dot -Tps < %s > %s" file_dot file_ps) in
  if e <> 0 then failwith "Ocamldot: error while dot was processing file"

let dot2pdf file_dot file_ps =
  let e = Sys.command (
      Printf.sprintf "dot -Tpdf < %s > %s" file_dot file_ps) in
  if e <> 0 then failwith "Ocamldot: error while dot was processing file"
