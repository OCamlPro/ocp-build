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

type state = {
  unit : unit;
}

module OCP_arg = struct

  type context = state

  let parse_error () = exit 2
  let new_file ctx filename digest = ()

  end

module EvalOCP2 = BuildOCP2Interp.Eval(OCP_arg)

let _ =
  for i = 1 to Array.length Sys.argv - 1 do
    let arg = Sys.argv.(i) in
    let state = { unit = () } in
    let config = BuildValue.empty_config () in
    let ( _ : BuildValue.TYPES.config) =
      EvalOCP2.read_ocamlconf arg state config in
    ()
  done
