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

(* Find an interface to deal with different severity of warnings.
For example, [equal] does only compare the [warnings] field,  so if
we create another field, equal will still not change.
*)

open OcpCompat

type set = {
  mutable warnings : string list;
  mutable count : int; (* length of [warnings] field *)
  mutable sorted : bool;
}

type 'a warning = set -> 'a -> unit


let empty_set () = { warnings = []; count = 0; sorted = true; }
let add w e =
  w.warnings <- e :: w.warnings;
  w.count <- w.count + 1;
  w.sorted <- false

let iter f w =
  List.iter f (List.rev w.warnings)
let count w = w.count
let sort w =
  if not w.sorted then begin
    w.warnings <- List.sort compare w.warnings;
    w.sorted <- true
  end

let rec equals list1 list2 =
  match list1, list2 with
  | [], [] -> true
  | h1 :: t1, h2 :: t2 ->
    (h1 : string) = h2 && equals t1 t2
  | _ -> false

let equal w1 w2 =
  w1.count = w2.count && (sort w1; sort w2; equals w1.warnings w2.warnings)
let copy w = { w with count = w.count }
let clear w =
  w.sorted <- true;
  w.count <- 0;
  w.warnings <- []

let diff w1 w2 =
  if w2.count = 0 then w1 else
    let w = empty_set () in
    let pre = Hashtbl.create 111 in
    List.iter (fun ww -> Hashtbl.add pre ww ()) w2.warnings;
    List.iter (fun ww ->
      if not (Hashtbl.mem pre ww) then add w ww
    ) w1.warnings;
    w

let wprintf w =
  Printf.kprintf (fun s -> add w s)
