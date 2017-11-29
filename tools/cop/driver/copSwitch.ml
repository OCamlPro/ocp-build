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

open OcpCompat
open BuildValue.TYPES
open CopTypes

let has_switch = ref false

let parse_switch c sw_name sw_env =
  if StringMap.mem sw_name c.c_switches then
    Printf.kprintf failwith "Switch %S: already exists" sw_name;
  let rec sw = {
      sw_name;
      sw_context = c;
      sw_host = sw;
      sw_build = sw;
      sw_packages = [];
    } in
  c.c_switches <- StringMap.add sw_name sw c.c_switches;
  BuildValue.iter_env (fun s v ->
      match s,v with
      | "host", VString (sw_host, _) ->
         begin try
             let sw_host = StringMap.find sw_host c.c_switches in
             sw.sw_host <- sw_host
           with Not_found ->
             Printf.kprintf failwith "Switch %S: host %S does not exist"
                            sw_name sw_host
         end
      | "build", VString (sw_host, _) ->
         begin try
             let sw_host = StringMap.find sw_host c.c_switches in
             sw.sw_host <- sw_host
           with Not_found ->
             Printf.kprintf failwith "Switch %S: host %S does not exist"
                            sw_name sw_host
         end
      | _ ->
         Printf.eprintf
           "Warning: useless field %S in declaration of switch %S\n%!"
           s sw_name
    ) sw_env;
  ()


let host_name pk =
  let _name = pk.pk_switch.sw_host.sw_name ^ ":" ^ pk.pk_name in
  assert false

type filter = {
    mutable fil_enabled : bool;
    fil_name : string;
    fil_pk : package option;
    mutable fil_needed_by : filter list;
    mutable fil_missing : filter list; (* missing dependencies *)
    mutable fil_broken : filter list;  (* broken dependencies *)
  }

let filter_packages packages =
  let table = Hashtbl.create 111 in

  (* prepare table *)
  List.iter (fun pk ->
      let fil = {
          fil_enabled = true;
          fil_name = host_name pk;
          fil_pk = Some pk;
          fil_needed_by = [];
          fil_broken = [];
          fil_missing = [];
        } in
      Hashtbl.add table (host_name pk) fil
    ) packages;

  (* propagate dependencies *)
  Hashtbl.iter (fun _ fil ->
      match fil.fil_pk with
      | None -> ()
      | Some pk ->
         List.iter (fun req ->
             let fil_dep =
               try
                 Hashtbl.find table req.req_name
               with Not_found ->
                 let fil_dep = {
                     fil_enabled = false;
                     fil_name = req.req_name;
                     fil_pk = None;
                     fil_needed_by = [];
                     fil_missing = [];
                     fil_broken = [];
                   } in
                 Hashtbl.add table req.req_name fil_dep;
                 fil_dep
             in
             fil_dep.fil_needed_by <- fil :: fil_dep.fil_needed_by;
           ) pk.pk_requires
    ) table;

  (* propagate missing/broken dependencies *)
  let rec disable fil =
    if fil.fil_enabled then begin
        fil.fil_enabled <- false;
        List.iter (fun fil2 ->
            fil2.fil_broken <- fil :: fil2.fil_broken;
            disable fil2
          ) fil.fil_needed_by
      end
  in
  Hashtbl.iter (fun _ fil ->
      match fil.fil_pk with
      | Some _ -> ()
      | None ->
         List.iter (fun fil2 ->
             fil2.fil_missing <- fil :: fil2.fil_missing;
             disable fil2
           ) fil.fil_needed_by
    ) table;

  (* filter correct/incorrect/missing packages *)
  let missing_packages = ref [] in
  let correct_packages = ref [] in
  let broken_packages = ref [] in
  Hashtbl.iter (fun _ fil ->
      match fil.fil_pk with
      | None ->
         missing_packages := fil :: !missing_packages
      | Some pk ->
         if fil.fil_enabled then
           correct_packages := pk :: !correct_packages
         else
           broken_packages := fil :: !broken_packages
    ) table;

  (* keep only correct dependencies *)
  !correct_packages,
  !broken_packages,
  !missing_packages

let sort_packages packages =
  let table = Hashtbl.create 111 in
  List.iter (fun pk ->
      Hashtbl.add table (host_name pk) pk
    ) packages;
  let module PackageSorter =
    OcpToposort.Make(
        struct
          type t = package
          let node pk = pk.pk_node
          let iter_edges f pk =
            List.iter (fun req ->
                f (Hashtbl.find table req.req_name)) pk.pk_requires
          let name pk = host_name pk
          let verbose n = n <= !CopGlobals.verbose
        end) in
  PackageSorter.sort packages

let eval_switch b sw nerrors packages =
  Printf.eprintf
    "Warning (switch %S): %d errors while evaluating project descriptions\n%!"
    sw.sw_name nerrors;
  (* TODO: validate packages (OCaml plugin) and other ones *)

  let packages, broken_packages, missing_packages = filter_packages packages in
  let nbroken = List.length broken_packages in
  let nmissing = List.length missing_packages in
  let npackages = List.length packages in
  Printf.eprintf "Packages: %d ok, %d broken (%d missing)\n%!"
                 npackages nbroken nmissing;
  let (sorted_packages, _cycle, _non_sorted) = sort_packages packages in
  List.iter (fun pk ->
      CopPackage.rules_of_package b sw pk
    ) sorted_packages;


  (* incomplete_packages, sort_packages, cycle, non_sorted *)
  ()

let has_switch () = !has_switch
