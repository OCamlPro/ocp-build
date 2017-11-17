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

let has_switch = ref false

type switch = {
    switch_name : string;
  }

let init switch_name config =
  has_switch := true;
  let s = {
      switch_name
    } in
  s, config

let eval_switch b s nerrors projects =
  Printf.eprintf
    "Warning (switch %S): %d errors while evaluating project descriptions\n%!"
    s.switch_name nerrors;
  ()

let has_switch () = !has_switch
