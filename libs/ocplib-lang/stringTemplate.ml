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

open OcpCompat

type t = {
  mutable content : string Lazy.t;
  subst : StringSubst.Static.t;
  labels : int StringMap.t;
  nargs : int;
  defaults : (string * string) list;
}
exception LabelMismatch of t * string
exception MissingLabel of t * string

let create content must defaults =
  let labels = ref StringMap.empty in
  let nargs = ref 0 in
  let args = ref [] in
  List.iter (fun s ->
    if not (StringMap.mem s !labels) then begin
      labels := StringMap.add s !nargs !labels;
      incr nargs;
      args := (Printf.sprintf "%%{%s}%%" s) :: !args;
    end) must;
  let defaults = match defaults with
    | None -> []
    | Some defaults ->
      List.iter (fun (s, _default) ->
        if not (StringMap.mem s !labels) then begin
          labels := StringMap.add s !nargs !labels;
          incr nargs;
          args := (Printf.sprintf "%%{%s}%%" s) :: !args;
        end) defaults;
      defaults
  in
  let subst = StringSubst.Static.create (Array.of_list (List.rev !args)) in
  {
    content = content;
    labels = !labels;
    subst = subst;
    nargs = !nargs;
    defaults = defaults;
  }

let load t args =
  let content = Lazy.force t.content in
  let targs = Array.make t.nargs None in
  List.iter (fun (label, value) ->
    try
      let pos = StringMap.find label t.labels in
      targs.(pos) <- Some value
    with Not_found ->
      raise (LabelMismatch (t, label))
  ) (t.defaults @ args);
  let targs = Array.mapi (fun i arg ->
      match arg with
      | Some s -> s
      | None ->
        StringMap.iter (fun label j ->
          if i = j then raise (MissingLabel (t, label))
        ) t.labels;
        assert false
    ) targs in
  snd (StringSubst.Static.subst t.subst targs content)

let labels t = StringMap.to_list_of_keys t.labels
let content t = Lazy.force t.content
