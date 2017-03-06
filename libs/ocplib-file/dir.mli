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

open StringCompat

val mkdir : File.t -> int -> unit

(* mkdir, with potentially any non-existing parent directory *)
val safe_mkdir : ?mode:int -> File.t -> unit

(* deprecated, use mkdir and mkdir_all *)
val make : File.t -> unit
val make_all : File.t -> unit

val list : File.t -> string list
val list_files : File.t -> File.t list
val iter : (string -> unit) -> File.t -> unit
val iter_files : (File.t -> unit) -> File.t -> unit
val remove : File.t -> unit
val remove_all : File.t -> unit
