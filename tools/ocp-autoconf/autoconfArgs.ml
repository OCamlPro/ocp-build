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

open OcpCompat

let arg_force = ref false
let arg_git_add = ref false
let arg_save_template = ref false

let ocp_autoconf_dir =  "ocp-autoconf.d"
let autoconf_dir =  "autoconf"
