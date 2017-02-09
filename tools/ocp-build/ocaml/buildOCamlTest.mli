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

type stats = {
  mutable tests_nsuccesses : int;
  mutable tests_nfailures : int;
  mutable tests_failures : (string * string) list;
  mutable tests_timings : (string * float) list;
}

val init : unit -> stats
val test_package :
  BuildEngineTypes.build_context ->
  stats -> BuildTypes.package_info ->
  bool -> (* benchmarks only ? *)
  unit
val finish : stats -> int -> unit
