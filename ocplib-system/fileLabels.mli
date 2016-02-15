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


(** Interface of File with labels. *)

(** Cut a filename at the last extension position *)
val cut_last_extension : basename:string -> string * string

(** Get the contents of a channel *)
val string_of_channel : ic:in_channel -> string

(** Output a line in a channel *)
val output_line : oc:out_channel -> str:string -> unit

(** Get all the lines of a file (possibly discarding some lines).  If
    [line_break] is set, '\n' are kept (default is false).*)
val lines_of_file :
  ?line_break:bool -> ?discard:(string -> bool)
  -> string -> string list

(** [file_of_lines name lines] saves the [lines] into the file [name] *)
val file_of_lines : filename:string -> string list -> unit

(** Get the contents of a file *)
val string_of_file : filename:string -> string

(** [file_of_string name str] saves [str] into the file [name]. *)
val file_of_string : filename:string -> str:string -> unit

(*
  val genlex_of_file : keywords:string list -> ?discard:(string -> bool)
  -> filename:string -> OcpGenlex.t
*)
