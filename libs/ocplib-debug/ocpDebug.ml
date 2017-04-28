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

let string_before s pos = String.sub s 0 pos
let string_after s pos =
  let len = String.length s in
  String.sub s pos (len-pos)

let cut_at s c =
  try
    let pos = String.index s c in
    string_before s pos, string_after s (pos+1)
  with _ -> s, ""

type debug_unit = {
    mutable unit_verbose : int;
    unit_sources : debug_source list;
  }

 and debug_source = {
     source_name : string;
     mutable source_verbose : int;
   }

let default_source = ""
let debug_units = ref []
let debug_sources = ref StringMap.empty
let output = ref (fun s -> Printf.eprintf "%s%!"s)

let verbose = ref (fun _ -> false)

let printf fmt = Printf.kprintf !output fmt

let get_debug_source source_name =
  try
    StringMap.find source_name !debug_sources
  with Not_found ->
       let s = {
           source_name;
           source_verbose = 0;
         } in
       debug_sources := StringMap.add source_name s !debug_sources;
       if !verbose 11 then
         printf "OcpDebug: new source %S\n" source_name;
       s

let verbose_function unit_sources =
  let unit_sources = List.map get_debug_source
                              (default_source :: unit_sources) in
  let debug_unit = {
      unit_verbose = 0; unit_sources;
    } in
  debug_units := debug_unit :: !debug_units;
  (function info_level ->
            debug_unit.unit_verbose >= info_level
  )

let () =
  verbose := verbose_function [ "OcpDebug" ]

let frozen_debug_units = ref false
let update_debug_units () =
  if not !frozen_debug_units then
    let rec iter_units units =
      match units with
        [] -> ()
      | unit :: units ->
         iter_sources unit units 0 unit.unit_sources

    and iter_sources unit units verbosity sources =
      match sources with
      | [] ->
         if unit.unit_verbose <> verbosity then
             unit.unit_verbose <- verbosity;
         iter_units units
      | s :: sources ->
         let verbosity = if s.source_verbose > verbosity then
                         s.source_verbose else verbosity
         in
         iter_sources unit units verbosity sources
    in
    iter_units !debug_units

let set_verbosity_source source_name n =
  let s = get_debug_source source_name in
  if !verbose 11 then
    printf "OcpDebug: source %S verbosity %d -> %d"
           s.source_name s.source_verbose n;
  s.source_verbose <- n;
  update_debug_units ()

let set_verbosity n = set_verbosity_source default_source n

let set_verbosity_parse s =
  frozen_debug_units := true;
  begin try
      let fields = split s ',' in
      List.iter (fun field ->
          try
            let source, level = cut_at field '=' in
            if level = "" then
              try
                let level = int_of_string field in
                set_verbosity level
              with _ ->
                let s = get_debug_source source in
                s.source_verbose <- 100
            else
              try
                let level = int_of_string level in
                set_verbosity_source source level
              with _ -> ()
          with _ -> ()
        ) fields
    with _ -> ()
  end;
  frozen_debug_units := false;
  update_debug_units ()

let set_verbosity_env s =
  try
      let s = Sys.getenv s in
      set_verbosity_parse s
  with Not_found -> ()

let register_output f = output := f

let () =
  set_verbosity_env "OCP_DEBUG"
