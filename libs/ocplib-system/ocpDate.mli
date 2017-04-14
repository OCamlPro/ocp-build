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

type iso8601

val iso8601 : unit -> iso8601

val iso8601_tm : MinUnix.tm -> iso8601

val string_of_iso8601 : iso8601 -> string

type timestamp

val timestamp : unit -> timestamp

val timestamp_tm : MinUnix.tm -> timestamp

val string_of_timestamp : timestamp -> string
