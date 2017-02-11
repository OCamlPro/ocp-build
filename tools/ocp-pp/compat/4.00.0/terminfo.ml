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

(* $Id: terminfo.ml 11156 2011-07-27 14:17:02Z doligez $ *)

(* Basic interface to the terminfo database *)

type status =
  | Uninitialised
  | Bad_term
  | Good_term of int
;;
external setup : out_channel -> status = "caml_terminfo_setup";;
external backup : int -> unit = "caml_terminfo_backup";;
external standout : bool -> unit = "caml_terminfo_standout";;
external resume : int -> unit = "caml_terminfo_resume";;
