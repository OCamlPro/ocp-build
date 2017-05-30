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

  open Subcommands.TYPES

  let arg_list = []
  let subcmd_spec = {
    subcmd_list = arg_list;
    subcmd_usage = [];
    subcmd_help = [];
  }
  let subcmd_main args = assert false
  let subcmd_init () = ()
