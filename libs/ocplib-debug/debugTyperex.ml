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

let split s c =
  let len = String.length s in
  let rec iter pos =
    if pos >= len then [] else
      try
        let pos2 = String.index_from s pos c in
        (String.sub s pos (pos2-pos)) :: iter (pos2+1)
      with Not_found -> [String.sub s pos (len-pos)]
  in
  iter 0

let modules = try
                split (Sys.getenv "OCP_DEBUG_MODULES") ' '
  with Not_found -> []

let projects = try
                 split (Sys.getenv "OCP_DEBUG_PROJECTS") ' '
  with Not_found -> []

module StringMap = Map.Make(String)

let map_of_list list =
  let map = ref StringMap.empty in
  List.iter (fun s ->
    Printf.fprintf stderr "DEBUG [%s]\n%!" s;
    try
      incr (StringMap.find s !map)
    with Not_found ->
      map := StringMap.add s (ref 1) !map) list;
  !map

let modules = ref (map_of_list modules)
let projects = ref (map_of_list projects)

let debug_module m = try StringMap.find m !modules with Not_found -> ref 0
let debug_project m = try StringMap.find m !projects with Not_found -> ref 0
