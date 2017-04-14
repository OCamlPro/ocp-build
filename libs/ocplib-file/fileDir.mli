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

val mkdir : FileAbs.t -> int -> unit

(* mkdir, with potentially any non-existing parent directory *)
val safe_mkdir : ?mode:int -> FileAbs.t -> unit

(* deprecated, use mkdir and mkdir_all *)
val make : FileAbs.t -> unit
val make_all : FileAbs.t -> unit

val list : FileAbs.t -> string list
val list_files : FileAbs.t -> FileAbs.t list
val iter : (string -> unit) -> FileAbs.t -> unit
val iter_files : (FileAbs.t -> unit) -> FileAbs.t -> unit
val remove : FileAbs.t -> unit
val remove_all : FileAbs.t -> unit
