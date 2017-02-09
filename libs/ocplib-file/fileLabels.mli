(**************************************************************************)
(*                                                                        *)
(*   Typerex Libraries                                                    *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Interface of File with labels. *)


(** Get all the lines of a file (possibly discarding some lines).  If
    [line_break] is set, '\n' are kept (default is false).*)
val lines_of_file : filename:string -> string array

(** [file_of_lines name lines] saves the [lines] into the file [name] *)
val file_of_lines : filename:string -> string array -> unit

(** Get the contents of a file *)
val string_of_file : filename:string -> string

(** [file_of_string name str] saves [str] into the file [name]. *)
val file_of_string : filename:string -> str:string -> unit

(*
  val genlex_of_file : keywords:string list -> ?discard:(string -> bool)
  -> filename:string -> OcpGenlex.t

(** Output a line in a channel *)
val output_line : oc:out_channel -> str:string -> unit

(** Get the contents of a channel *)
val string_of_channel : ic:in_channel -> string

(** Cut a filename at the last extension position *)
val cut_at_last_extension : basename:string -> string * string

*)
