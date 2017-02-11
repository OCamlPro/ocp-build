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

open MinUnix

type iso8601 = string

let iso8601_tm t =
  Printf.sprintf
    "%04d%02d%02dT%02d:%02d:%02dZ"
    (1900 + t.tm_year)
    (1 + t.tm_mon)
    t.tm_mday
    t.tm_hour
    t.tm_min
    t.tm_sec

let iso8601 () =
  iso8601_tm (gmtime (time ()))

let string_of_iso8601 x = x

type timestamp = string

let timestamp_tm t =
  Printf.sprintf
    "%04d%02d%02d%02d%02d%02d"
    (1900 + t.tm_year)
    (1 + t.tm_mon)
    t.tm_mday
    t.tm_hour
    t.tm_min
    t.tm_sec

let timestamp () =
  timestamp_tm (gmtime (time ()))

let string_of_timestamp x = x
