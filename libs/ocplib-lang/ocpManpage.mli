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

type 'div man_page = {
    man_name : string;   (* "OPAM" "GCC" *)
    man_section : int;
    man_date : string;
    man_release : string; (* "SOFT VERSION" *)
    man_org : string; (* GNU, OPAM Manual *)
    man_text : 'div list;
}

module CMDLINER : sig

  type man_block =
  | S of string
  | P of string
  | I of string * string
  | NOBLANK

  type pager =
  | PLAIN
  | PAGER
  | GROFF

  val print :
    ?subst:(string -> string) -> pager ->
    Format.formatter ->
    man_block man_page -> unit

end

module RAW : sig

  type div =
    | SH of span list
    | LI of span list * span list
    | P of span list
    | P2 of span list

  and span =
    | S of string
    | B of string
    | I of string

  val groff_page : div man_page -> string

end
