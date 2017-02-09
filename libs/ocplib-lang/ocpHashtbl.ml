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

open Hashtbl

let to_list h =
  fold (fun k v accu -> (k,v) :: accu) h []

let of_list l =
  let h = create (List.length l) in
  List.iter (fun (k,v) -> add h k v) l;
  h

let incr h key =
  if mem h key then
    let n = find h key in
    replace h key (n+1)
  else
    add h key 1

let for_all h fn =
  fold (fun k v accu -> accu && fn k v) h true

let exists h fn =
  fold (fun k v accu -> accu || fn k v) h false
