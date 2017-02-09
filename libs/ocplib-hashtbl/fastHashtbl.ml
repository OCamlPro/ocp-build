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

type ('a, 'b) t = {
  mutable lookup_table_size : int;
  mutable storage_table_size : int;
  mutable storage_free_slot : int;
  mutable length : int;
  mutable lookup_table : int array;
  mutable storage_table_keys : 'a array;
  mutable storage_table_values : 'b array;
  mutable storage_table_next : int array;
}

let not_found = Not_found

let create initial_size =
  let lookup_initial_size = min (max 13 initial_size) Sys.max_array_length in
  {
    lookup_table_size = lookup_initial_size;
    storage_table_size = 0;
    storage_free_slot = (-1);
    length = 0;
    lookup_table = Array.make lookup_initial_size (-1);
    storage_table_keys = [||];
    storage_table_values = [||];
    storage_table_next = [||];
  }

let clear h =
  for i = 0 to h.lookup_table_size - 1 do
    h.lookup_table.(i) <- (-1);
  done;
  h.length <- 0;
  h.storage_table_size <- 0;
  h.storage_table_keys <- [||];
  h.storage_table_values <- [||];
  h.storage_table_next <- [||]

let length h = h.length

let addr_repr = Obj.repr (Some 0)
let float_repr = Obj.repr (3.)

let is_float v =
  let v = Obj.repr v in
  Obj.is_block v && Obj.tag v = Obj.double_tag

let representant v =
  if is_float v then
    Obj.magic float_repr
  else
    Obj.magic addr_repr

let copy h =
  { h with
    lookup_table = Array.copy h.lookup_table;
    storage_table_keys = Array.copy h.storage_table_keys;
    storage_table_values = Array.copy h.storage_table_values;
    storage_table_next = Array.copy h.storage_table_next;
  }


(* Functorial interface *)

module type HashedType =
  sig
    type t
    val equal: t -> t -> bool
    val hash: t -> int
  end

module type S =
  sig
    type key
    type 'a t
    val create: int -> 'a t
    val clear: 'a t -> unit
    val copy: 'a t -> 'a t
    val add: 'a t -> key -> 'a -> unit
    val remove: 'a t -> key -> unit
    val find: 'a t -> key -> 'a
    val find_all: 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length: 'a t -> int
  end

module Make(H: HashedType): (S with type key = H.t) =
  struct
    type key = H.t
    type 'a hashtbl = (key, 'a) t
    type 'a t = 'a hashtbl
    let create = create
    let clear = clear
    let copy = copy
    let length = length

    let hash key = (H.hash key) land max_int
    let equal = H.equal

let rec mem_rec h key num =
  if num < 0 then false else
  if equal key h.storage_table_keys.(num) then
    true
  else
    mem_rec h key h.storage_table_next.(num)

let mem h key =
  let num = (hash key) mod h.lookup_table_size in
  mem_rec h key h.lookup_table.(num)

let rec find_rec h key num =
  if num < 0 then raise not_found;
  if equal key h.storage_table_keys.(num) then
    h.storage_table_values.(num)
  else
    find_rec h key h.storage_table_next.(num)

let find h key =
  let hash = (hash key) mod h.lookup_table_size in
  find_rec h key h.lookup_table.(hash)

let rec find_all_rec h key num = (* not tail recursive *)
  if num < 0 then [] else
  if equal key h.storage_table_keys.(num) then
    h.storage_table_values.(num) :: find_all_rec h key h.storage_table_next.(num)
  else
    find_all_rec h key h.storage_table_next.(num)

let find_all h key =
  let hash = (hash key) mod h.lookup_table_size in
  find_all_rec h key h.lookup_table.(hash)

let rec iter_rec f h num =
  if num < 0 then () else
    let key = h.storage_table_keys.(num) in
    let v = h.storage_table_values.(num) in
    f key v;
    let num = h.storage_table_next.(num) in
    iter_rec f h num

let iter f h =
  for i = 0 to h.lookup_table_size - 1 do
    iter_rec f h h.lookup_table.(i)
  done

let rec resize h key v =
  if h.storage_table_size = 0 then begin

    let storage_table_size = h.lookup_table_size / 2 in
(*    Printf.fprintf stderr "initial storage_table_size to %d\n" storage_table_size; *)
    let storage_table_next = Array.make storage_table_size 0 in
    let storage_table_keys = Array.make storage_table_size (representant key) in
    let storage_table_values = Array.make storage_table_size (representant v) in
    for i = 0 to storage_table_size - 2 do
      storage_table_next.(i) <- (i+1);
    done;
    storage_table_next.(storage_table_size-1) <- (-1);

    h.storage_table_size <- storage_table_size;
    h.storage_table_next <- storage_table_next;
    h.storage_table_keys <- storage_table_keys;
    h.storage_table_values <- storage_table_values;
    h.storage_free_slot <- 0;

  end else
    let lookup_table_size = min (h.lookup_table_size * 2 + 1) Sys.max_array_length in
    if h.lookup_table_size < lookup_table_size then begin

      let storage_table_size = lookup_table_size / 2 in
      let lookup_table = Array.make lookup_table_size (-1) in
(*      Printf.fprintf stderr "resize storage_table_size to %d\n" storage_table_size; *)
      let storage_table_next = Array.make storage_table_size 0 in
      let storage_table_keys = Array.make storage_table_size (representant key) in
      let storage_table_values = Array.make storage_table_size (representant v) in
      for i = 0 to storage_table_size - 2 do
   storage_table_next.(i) <- (i+1);
      done;
      storage_table_next.(storage_table_size-1) <- (-1);

      let old_keys = h.storage_table_keys in
      let old_values = h.storage_table_values in

      h.lookup_table <- lookup_table;
      h.lookup_table_size <- lookup_table_size;

      h.storage_table_size <- storage_table_size;
      h.storage_table_next <- storage_table_next;
      h.storage_table_keys <- storage_table_keys;
      h.storage_table_values <- storage_table_values;
      h.storage_free_slot <- 0;

      for i = 0 to Array.length old_keys - 1 do
   add h old_keys.(i) old_values.(i)
      done;

    end else
      let max_array_length =
        if is_float key || is_float v then
          Sys.max_array_length / 2
        else Sys.max_array_length in
        if h.storage_table_size < max_array_length then begin

          let storage_table_size = max_array_length in
       let storage_table_next = Array.make storage_table_size 0 in
     let storage_table_keys = Array.make storage_table_size (representant key) in
     let storage_table_values = Array.make storage_table_size (representant v) in
     for i = h.storage_table_size to storage_table_size - 2 do
       storage_table_next.(i) <- (i+1);
     done;
     storage_table_next.(storage_table_size-1) <- (-1);
     for i = 0 to h.storage_table_size - 1 do
       storage_table_next.(i) <- h.storage_table_next.(i);
       storage_table_keys.(i) <- h.storage_table_keys.(i);
       storage_table_values.(i) <- h.storage_table_values.(i);
     done;
     h.storage_free_slot <- h.storage_table_size;

     h.storage_table_size <- storage_table_size;
     h.storage_table_next <- storage_table_next;
     h.storage_table_keys <- storage_table_keys;
     h.storage_table_values <- storage_table_values;

   end else
     failwith "Hashtbl: Cannot grow storage table size"

and add h key v =
  if h.storage_free_slot < 0 then resize h key v;
  let num = h.storage_free_slot in
  h.storage_free_slot <- h.storage_table_next.(num);
  let hash = (hash key) mod h.lookup_table_size in
  let old_num = h.lookup_table.(hash) in
  h.lookup_table.(hash) <- num;
  h.storage_table_next.(num) <- old_num;
  h.storage_table_values.(num) <- v;
  h.storage_table_keys.(num) <- key;
  h.length <- h.length + 1
;;

let rec remove_rec h key num =
  if num < 0 then num else
    let next_num = h.storage_table_next.(num) in
    if equal key h.storage_table_keys.(num) then begin
      h.storage_table_values.(num) <- representant h.storage_table_values.(num);
      h.storage_table_keys.(num) <- representant key;
      h.storage_table_next.(num) <- h.storage_free_slot;
      h.storage_free_slot <- num;
      remove_rec h key next_num
    end else begin
      h.storage_table_next.(num) <- remove_rec h key next_num;
      num
    end

let remove h key =
  let num = (hash key) mod h.lookup_table_size in
  h.lookup_table.(num) <- remove_rec h key h.lookup_table.(num)


let rec replace_rec h key v num =
  if num < 0 then raise not_found;
  if equal key h.storage_table_keys.(num) then
    h.storage_table_values.(num) <- v
  else
    replace_rec h key v h.storage_table_next.(num)

let replace h key v =
  let hash = (hash key) mod h.lookup_table_size in
  replace_rec h key v h.lookup_table.(hash)


let rec fold_rec f h num accu =
  if num < 0 then accu else
    let key = h.storage_table_keys.(num) in
    let v = h.storage_table_values.(num) in
    let accu = f key v accu in
    let num = h.storage_table_next.(num) in
    fold_rec f h num accu

let fold f h init =
  let accu = ref init in
  for i = 0 to h.lookup_table_size - 1 do
    accu := fold_rec f h h.lookup_table.(i) !accu
  done;
  !accu

end

external hash_param : int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"

let hash x = hash_param 10 100 x

let equal x y =
  compare x y = 0


let rec mem_rec h key num =
  if num < 0 then false else
  if equal key h.storage_table_keys.(num) then
    true
  else
    mem_rec h key h.storage_table_next.(num)

let mem h key =
  let num = (hash key) mod h.lookup_table_size in
  mem_rec h key h.lookup_table.(num)

let rec find_float_rec h (key : float) num =
  if num < 0 || key = h.storage_table_keys.(num) then
    num
  else
    find_float_rec h key h.storage_table_next.(num)

let rec find_nonfloat_rec h (key : int option) num =
  if num < 0 || compare key h.storage_table_keys.(num) = 0 then
    num
  else
    find_nonfloat_rec h key h.storage_table_next.(num)

(*
let rec find_rec h key num =
  if num < 0 || equal key h.storage_table_keys.(num) then
    num
  else
    find_rec h key h.storage_table_next.(num)
*)

let find h key =
  let num =
    if is_float key then
      find_float_rec (Obj.magic h) (Obj.magic key) h.lookup_table.( (hash key) mod h.lookup_table_size)
    else
      find_nonfloat_rec (Obj.magic h) (Obj.magic key) h.lookup_table.((hash key) mod h.lookup_table_size)
  in
  if num < 0 then
    raise not_found
  else
    h.storage_table_values.(num)

let rec find_all_rec h key num = (* not tail recursive *)
  if num < 0 then [] else
  if equal key h.storage_table_keys.(num) then
    h.storage_table_values.(num) :: find_all_rec h key h.storage_table_next.(num)
  else
    find_all_rec h key h.storage_table_next.(num)

let find_all h key =
  let hash = (hash key) mod h.lookup_table_size in
  find_all_rec h key h.lookup_table.(hash)

let rec iter_rec f h num =
  if num < 0 then () else
    let key = h.storage_table_keys.(num) in
    let v = h.storage_table_values.(num) in
    f key v;
    let num = h.storage_table_next.(num) in
    iter_rec f h num

let iter f h =
  for i = 0 to h.lookup_table_size - 1 do
    iter_rec f h h.lookup_table.(i)
  done

let rec resize h key v =
  if h.storage_table_size = 0 then begin

    let storage_table_size = h.lookup_table_size / 2 in
(*    Printf.fprintf stderr "initial storage_table_size to %d\n" storage_table_size; *)
    let storage_table_next = Array.make storage_table_size 0 in
    let storage_table_keys = Array.make storage_table_size (representant key) in
    let storage_table_values = Array.make storage_table_size (representant v) in
    for i = 0 to storage_table_size - 2 do
      storage_table_next.(i) <- (i+1);
    done;
    storage_table_next.(storage_table_size-1) <- (-1);

    h.storage_table_size <- storage_table_size;
    h.storage_table_next <- storage_table_next;
    h.storage_table_keys <- storage_table_keys;
    h.storage_table_values <- storage_table_values;
    h.storage_free_slot <- 0;

  end else
    let lookup_table_size = min (h.lookup_table_size * 2 + 1) Sys.max_array_length in
    if h.lookup_table_size < lookup_table_size then begin

      let storage_table_size = lookup_table_size / 2 in
      let lookup_table = Array.make lookup_table_size (-1) in
(*      Printf.fprintf stderr "resize storage_table_size to %d\n" storage_table_size; *)
      let storage_table_next = Array.make storage_table_size 0 in
      let storage_table_keys = Array.make storage_table_size (representant key) in
      let storage_table_values = Array.make storage_table_size (representant v) in
      for i = 0 to storage_table_size - 2 do
   storage_table_next.(i) <- (i+1);
      done;
      storage_table_next.(storage_table_size-1) <- (-1);

      let old_keys = h.storage_table_keys in
      let old_values = h.storage_table_values in

      h.lookup_table <- lookup_table;
      h.lookup_table_size <- lookup_table_size;

      h.storage_table_size <- storage_table_size;
      h.storage_table_next <- storage_table_next;
      h.storage_table_keys <- storage_table_keys;
      h.storage_table_values <- storage_table_values;
      h.storage_free_slot <- 0;

      for i = 0 to Array.length old_keys - 1 do
   add h old_keys.(i) old_values.(i)
      done;

    end else
      let max_array_length =
        if is_float key || is_float v then
          Sys.max_array_length / 2
        else Sys.max_array_length in
        if h.storage_table_size < max_array_length then begin

          let storage_table_size = max_array_length in
     let storage_table_next = Array.make storage_table_size 0 in
     let storage_table_keys = Array.make storage_table_size (representant key) in
     let storage_table_values = Array.make storage_table_size (representant v) in
     for i = h.storage_table_size to storage_table_size - 2 do
       storage_table_next.(i) <- (i+1);
     done;
     storage_table_next.(storage_table_size-1) <- (-1);
     for i = 0 to h.storage_table_size - 1 do
       storage_table_next.(i) <- h.storage_table_next.(i);
       storage_table_keys.(i) <- h.storage_table_keys.(i);
       storage_table_values.(i) <- h.storage_table_values.(i);
     done;
     h.storage_free_slot <- h.storage_table_size;

     h.storage_table_size <- storage_table_size;
     h.storage_table_next <- storage_table_next;
     h.storage_table_keys <- storage_table_keys;
     h.storage_table_values <- storage_table_values;

   end else
     failwith "Hashtbl: Cannot grow storage table size"

and add h key v =
  if h.storage_free_slot < 0 then resize h key v;
  let num = h.storage_free_slot in
  h.storage_free_slot <- h.storage_table_next.(num);
  let hash = (hash key) mod h.lookup_table_size in
  let old_num = h.lookup_table.(hash) in
  h.lookup_table.(hash) <- num;
  h.storage_table_next.(num) <- old_num;
  h.storage_table_values.(num) <- v;
  h.storage_table_keys.(num) <- key;
  h.length <- h.length + 1
;;

let rec remove_rec h key num =
  if num < 0 then num else
    let next_num = h.storage_table_next.(num) in
    if equal key h.storage_table_keys.(num) then begin
      h.storage_table_values.(num) <- representant h.storage_table_values.(num);
      h.storage_table_keys.(num) <- representant key;
      h.storage_table_next.(num) <- h.storage_free_slot;
      h.storage_free_slot <- num;
      remove_rec h key next_num
    end else begin
      h.storage_table_next.(num) <- remove_rec h key next_num;
      num
    end

let remove h key =
  let num = (hash key) mod h.lookup_table_size in
  h.lookup_table.(num) <- remove_rec h key h.lookup_table.(num)


let rec replace_rec h key v num =
  if num < 0 then raise not_found;
  if equal key h.storage_table_keys.(num) then
    h.storage_table_values.(num) <- v
  else
    replace_rec h key v h.storage_table_next.(num)

let replace h key v =
  let hash = (hash key) mod h.lookup_table_size in
  replace_rec h key v h.lookup_table.(hash)


let rec fold_rec f h num accu =
  if num < 0 then accu else
    let key = h.storage_table_keys.(num) in
    let v = h.storage_table_values.(num) in
    let accu = f key v accu in
    let num = h.storage_table_next.(num) in
    fold_rec f h num accu

let fold f h init =
  let accu = ref init in
  for i = 0 to h.lookup_table_size - 1 do
    accu := fold_rec f h h.lookup_table.(i) !accu
  done;
  !accu
