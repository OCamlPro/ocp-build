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

(* Fabrice: we use a very basic scheme in O(M.N) where M is the size
   of the file and N the size of the longest key. We store keys in a
   prefix-tree, and search the prefix tree at each file position.

   If we need to be more efficient, we should probably use KMP or
   something like that.
*)

let not_found = Not_found

module CharMap = Map.Make(struct
    type t = char
    let compare x y = Char.code x - Char.code y end)

module ExplicitSubst = struct

  type 'a t = {
    mutable map : 'a t CharMap.t;
    mutable result : ('a -> string) option;
  }

  let rec add_prefix_map key pos v node =
    if pos < String.length key then
      let c = key.[pos] in
      let node =
        try CharMap.find c node.map with Not_found ->
          let new_node = { map = CharMap.empty; result = None } in
          node.map <- CharMap.add c new_node node.map;
          new_node
      in
      add_prefix_map key (pos+1) v node
    else
      node.result <- Some v

  let empty_subst () = { map = CharMap.empty; result = None }

  let add_to_subst subst key v =
    add_prefix_map key 0 v subst

  let add_to_copy subst key v =
    let rec add_to_copy key pos v node =
      if pos < String.length key then
        let c = key.[pos] in
        let new_node =
          try CharMap.find c node.map with Not_found ->
            { map = CharMap.empty; result = None }
        in
        let new_node = add_to_copy key (pos+1) v new_node in
        { node with
          map = CharMap.add c new_node node.map }
      else
        { node with result = Some v }
    in
    add_to_copy key 0 v subst

  let subst_of_list list =
    let c = empty_subst () in
    List.iter (fun (key,v) ->
      add_prefix_map key 0 v c;
    ) list;
    c

  let rec find_in_map b info node s pos =
    (*  Printf.fprintf stderr "find_in_map %d/%d\n" pos (String.length s); *)
    if pos < String.length s then
      match node.result with
        None ->
        let node = CharMap.find s.[pos] node.map in
        find_in_map b info node s (pos+1)
      | Some repl ->
        try
          let node = CharMap.find s.[pos] node.map in
          find_in_map b info node s (pos+1)
        with Not_found ->
          Buffer.add_string b (repl info);
          pos
    else
      match node.result with
        None -> raise not_found
      | Some repl ->
        Buffer.add_string b (repl info);
        pos

  let subst config s info =
    let b = Buffer.create (String.length s) in
    let has_subst = ref 0 in
    let len = String.length s in
    let rec iter pos =
      (*    Printf.fprintf stderr "current_pos %d\n" pos; *)
      if pos < len then
        try
          let next_pos = find_in_map b info config s pos in
          incr has_subst;
          iter next_pos
        with Not_found ->
          Buffer.add_char b s.[pos];
          iter (pos+1)
    in
    iter 0;
    !has_subst, Buffer.contents b

  let iter_subst config s info =
    let nsubst = ref 0 in
    let rec iter s =
      let has_subst, s = subst config s info in
      nsubst := !nsubst + has_subst;
      if has_subst > 0 then iter s else !nsubst, s
    in
    iter s


end

module M = struct
  include ExplicitSubst

  type 'a subst = 'a t
end

module Static = struct

  open ExplicitSubst

  type t = string array ExplicitSubst.t

  let create (strings : string array) =
    let t = empty_subst () in
    for i = 0 to Array.length strings - 1 do
      add_to_subst t strings.(i) (fun tab -> tab.(i))
    done;
    t

  let subst t strings string = ExplicitSubst.subst t string strings
  let iter_subst t strings string =
    snd (ExplicitSubst.iter_subst t string strings)

end

module Gen = struct

  include ExplicitSubst
  type subst = unit t

  let add_to_subst t s string = add_to_subst t s (fun () -> string)
  let add_to_copy t s string = add_to_copy t s (fun () -> string)
  let subst_of_list list =
    subst_of_list (List.map (fun (s1, s2) -> s1, function () -> s2) list)
  let subst sb string = subst sb string ()
  let iter_subst sb string = iter_subst sb string ()

end

include Gen
