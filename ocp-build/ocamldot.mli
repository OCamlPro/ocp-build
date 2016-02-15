(**************************************************************************)
(*                                                                        *)
(*                              OCamlPro TypeRex                          *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*      (GNU Public Licence version 3.0).                                 *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)


type graph =
  { graph_name: string;
    mutable graph_nodes: node list;
    mutable graph_edges: edge list;
    mutable node_counter: int;
    graph_attributes: graph_attributes list }
and node =
  { mutable node_name: string;
    node_id: int;
    node_graph: graph;
    node_attributes: node_attributes list }
and edge =
  { edge_from: node;
    edge_to: node;
    edge_attributes: edge_attributes list }
and graph_attributes = | GraphSize of float * float
and node_attributes =
  | NodeColor of string
  | NodeFontColor of string
  | NodeFontName of string
  | NodeShape of shape
  | NodeHeight of float
  | NodeWidth of float
  | NodeStyle of goptions
and edge_attributes =
  | EdgeDirection of direction
  | EdgeLabel of string
  | EdgeStyle of goptions
  | EdgeWeight of int
and direction = | Forward | Backward | Bothdir | Nodir
and goptions = | Bold | Dotted | Filled
and shape =
  | Ellipse
  | Box
  | Circle
  | DoubleCircle
  | Diamond
  | PlainText
  | Record
  | Polygon of int * polygon_options list
  | Epsf of string
and polygon_options = | Skew of float | Distortion of float
val create : string -> graph_attributes list -> graph
val node : graph -> string -> node_attributes list -> node
val edge : node -> node -> edge_attributes list -> edge
val add_edge : node -> node -> edge_attributes list -> unit
val add_edges : node -> node list -> edge_attributes list -> unit
val add_path : node list -> edge_attributes list -> unit
val rename_node : node -> string -> unit
val save : graph -> string -> unit
val save_in : graph -> out_channel -> unit
val view : graph -> unit
val dot2ps : string -> string -> unit
val dot2pdf : string -> string -> unit
