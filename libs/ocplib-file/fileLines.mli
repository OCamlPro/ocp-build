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

(** Get all the lines of a files *)
val read_file : string -> string list

(** [file_of_lines name lines] saves the [lines] into the file [name] *)
val write_file : string -> string list -> unit

(** [iter_lines f filename] reads [filename] line by line, applying [f] to each one. *)
val iter : (string -> unit) -> string -> unit

(** [iteri_lines f filename] reads [filename] line by line, applying [f] to each one. *)
val iteri : (int -> string -> unit) -> string -> unit

(** [sub_lines filename off len] returns [len] lines of [filename], starting at [off] *)
val sub : string -> int -> int -> string list
