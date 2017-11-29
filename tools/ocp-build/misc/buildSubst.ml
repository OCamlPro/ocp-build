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

open OcpSubst

  (*
let global_subst = empty_subst ()

let add_to_subst env var vv =
(*  Printf.eprintf "BuildSubst.add %S -> %S\n%!" v vv; *)
  add_to_subst env  (Printf.sprintf "%%{%s}%%" var) vv

let add_to_global_subst var var_value =
  add_to_subst global_subst var var_value

let _ =
  Array.iter (fun s ->
    let var, var_value = OcpString.cut_at s '=' in
    add_to_global_subst var var_value;
  ) (MinUnix.environment ())

let subst env_subst s =
  let ss = snd (iter_subst env_subst s) in
(*  Printf.eprintf "BuildSubst.subst %S -> %S\n%!" s ss; *)
  ss

let subst_global = subst global_subst

let add_to_local_subst env var vv =
  add_to_copy env  (Printf.sprintf "%%{%s}%%" var) vv

let create_substituter list =
  let subst = M.empty_subst () in
  List.iter (fun (name, f) ->
    M.add_to_subst subst ("%{" ^ name ^ "}%") f
  ) list;
  subst

let apply_substituter subst s info =
  let _, s1 = M.iter_subst subst s info in
(*  Printf.eprintf "apply_substituter: %S -> %S\n%!" s s1; *)
  s1
  *)





type 'context t = ('context -> string -> string)

let substitute f context s =
  (* Printf.eprintf "BuildSubst.substitute %S\n%!" s; *)
  let len = String.length s in
  let b = Buffer.create len in

  let rec iter b stack i =
    if i = len then
      match stack with
      | [] -> ()
      | b1 :: stack ->
        Buffer.add_string b1 "%{";
        Buffer.add_string b1 (Buffer.contents b);
        iter b1 stack i
    else
      match s.[i] with
      | '%' -> iter1 b stack (i+1)
      | '}' when stack != [] -> iter2 b stack (i+1)
      | c ->
        Buffer.add_char b c;
        iter b stack (i+1)

  and iter1 b stack i =
    if i = len then begin
      Buffer.add_char b '%';
      iter b stack i
    end
    else
      match s.[i] with
      | '%' ->
        Buffer.add_char b '%';
        iter b stack (i+1)
      | '{' ->
        iter (Buffer.create len) (b :: stack) (i+1)
      | c ->
        Buffer.add_char b '%';
        Buffer.add_char b c;
        iter b stack (i+1)

  and iter2 b stack i =
    if i = len then
      match stack with
      | [] -> assert false
      | b1 :: stack ->
        Buffer.add_string b1 ("%{" ^ Buffer.contents b ^ "}");
        iter b1 stack i
    else
      match s.[i] with
      | '%' -> begin
        match stack with
        | [] -> assert false
        | b1 :: stack ->
          let ident = Buffer.contents b in
          let replacement = f context ident in
          Buffer.add_string b1 replacement;
          iter b1 stack (i+1)
      end
      | _ ->
        Buffer.add_char b '}';
        iter b stack i
  in
  iter b [] 0;
  let s1 = Buffer.contents b in
  (*  Printf.eprintf "subst %S = %S\n%!" s s1; *)
  s1

let sub = substitute (fun context s ->
  match s with
  | "toto" -> "TOTO"
  | "tutu" -> context
  | x -> "<" ^ x ^ ">") "to"

let () =
  assert (sub "%{toto}%" = "TOTO");
  assert (sub "%{tutu}%" = "to");
  assert (sub "%{toto}%%{tutu}%%{toto}%" = "TOTOtoTOTO");
  assert (sub "%{to%{tutu}%}%%{tutu}%%{toto}%" = "TOTOtoTOTO");
  assert (sub "%{toto" = "%{toto");
  assert (sub "%{toto}" = "%{toto}");
  assert (sub "%{toto}{}%" = "<toto}{>");
  assert (sub "%{" = "%{");
  assert (sub "%%" = "%");
  ()

open OcpCompat

let map_subst map s =
  substitute (fun map s ->
    try
      StringMap.find s map
    with Not_found -> "%{" ^ s ^ "}%") map s

let global_subst = ref StringMap.empty

(*
let add_to_subst env var vv = StringMap.add var vv
 *)

let add_to_global_subst var var_value =
  global_subst := StringMap.add var var_value !global_subst

let _ =
  Array.iter (fun s ->
    let var, var_value = OcpString.cut_at s '=' in
    add_to_global_subst var var_value;
  ) (MinUnix.environment ())


let subst_global s = map_subst !global_subst s

                               (*
let add_to_local_subst env var vv =
  add_to_copy env  (Printf.sprintf "%%{%s}%%" var) vv
                                *)

let create_substituter list =
  let map = ref StringMap.empty in
  List.iter (fun (name, f) ->
    map := StringMap.add name f !map
  ) list;
  fun context s ->
    try
      (StringMap.find s !map) context
    with Not_found -> "%{" ^ s ^ "}%"

let apply_substituter subst s context =
  substitute subst context s

let putenv var var_value =
  MinUnix.putenv var var_value;
  add_to_global_subst var var_value

let global_subst () = !global_subst
