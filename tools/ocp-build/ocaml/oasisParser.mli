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

type oasis_line = Line of string * oasis_line list ref

val parse_oasis : oasis_line list -> unit

val read_oasis : string -> oasis_line list
val print_oasis : out_channel -> oasis_line list -> unit
