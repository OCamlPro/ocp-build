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



(* TODO: we should save the version of ocaml used to build a project,
   so that we can detect changes and ask for a clean before building.
   Can we access the magic used by every compiler ? (we can compile an
   empty file in bytecode and native code) We could cache this
   information using the uniq identifier of the executable (would not
   work with wrappers).
*)

(* TODO
  We could force packages with missing dependencies to still be computed,
  since it is still possible that these missing dependencies are not used
  in a particular compilation scheme.
*)

open OcpCompat




open BuildEngineTypes



open BuildGlobals


let verbose = OcpDebug.verbose_function ["B"; "BuildMain" ]

let initial_verbosity =
  OcpDebug.set_verbosity_env "OCPBUILD_VERBOSITY"

let version = BuildVersion.version


let t0 = MinUnix.gettimeofday ()

let time s f x =
  if !time_arg then
    let t0 = MinUnix.gettimeofday () in
    let y = f x in
    let t1 = MinUnix.gettimeofday () in
    Printf.printf s (t1 -. t0);
    y
  else
    f x

(* TODO: check which options are still settable *)
let add_external_projects_arg = ref ([] : string list)
let oasis_arg = ref false
let save_project = ref false
let print_conflicts_arg = ref false
let list_projects_arg = ref false
let meta_verbose_arg = ref false
let build_dir_basename_arg = ref "_obuild"
type arch_arg = ArchNone | Arch of string
let arch_arg = ref ArchNone

let benchmarks_arg = ref false
let configure_arg = ref false

(* if [query_global] is set, we don't load the project
.ocp files and stop immediatly after replying to queries. *)
let query_global = ref false
let query_root_dir = ref false
let query_install_dir = ref (None : string option)
let query_include_dir = ref ([] : string list)
let query_has_package_args = ref ([] : string list)

let delete_orphans_arg = ref DeleteOrphanFilesAndDirectories
let list_installed_arg = ref false
let install_arg = ref false
let uninstall_arg = ref false

let auto_uninstall = ref true

let arg_anon s =  BuildGlobals.targets_arg := s :: !BuildGlobals.targets_arg
