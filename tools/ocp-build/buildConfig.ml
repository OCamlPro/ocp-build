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

(* Génération et lecture du fichier de configuration *)


open OcpCompat

(* let verbose = DebugVerbosity.verbose [ "B" ] "BuildConfig" *)


let sep_PATH =
  match Sys.os_type with
    "Win32" -> ';'
  | _ -> ':'

let sep_PATH_str = String.make 1 sep_PATH

let get_PATH () =
  try
    let path = Sys.getenv "PATH" in
    OcpString.split path sep_PATH
  with Not_found ->
    failwith "Env variable PATH not defined"

let set_PATH path =
  MinUnix.putenv "PATH" (String.concat sep_PATH_str path)

let check_command_exists filename =
  let st = MinUnix.stat filename in
  match st.MinUnix.st_kind with
    MinUnix.S_REG ->
      begin
        try
          MinUnix.access filename [MinUnix.X_OK];
          filename
        with e ->
          Printf.eprintf "Warning: %s in PATH has not executable permission\n%!"
            filename;
          raise e
      end
  | _ ->
    Printf.eprintf "Warning: %s in PATH is not a regular command\n%!" filename;
    raise Not_found

let rec find_in_PATH command path =
  match path with
    [] -> raise Not_found
  | dirname :: path ->
    let filename = Filename.concat dirname command in
    try
      check_command_exists filename
    with _ ->
      if MinUnix.os_type = MinUnix.WINDOWS ||
         MinUnix.os_type = MinUnix.CYGWIN then
        try
          check_command_exists (filename ^ ".exe")
        with _ ->
          find_in_PATH command path
      else
        find_in_PATH command path

let rec find_first_in_path path filter list =
  match list with
    [] -> None
  | basename :: others ->
    try
      let binary = find_in_PATH basename path in
      if filter binary then Some binary else raise Not_found
    with Not_found ->
      find_first_in_path path filter others

let split_version version =
  let version = Bytes.of_string version in
  for i = 0 to Bytes.length version - 1 do
    match Bytes.get version i with
      '0'..'9' -> ()
    | _ -> version.[i] <- ' '
  done;
  let version = Bytes.to_string version in
  match OcpString.split_simplify version ' ' with
  | [ major ] -> (major, "00", "0")
  | [ major; minor ] -> (major, minor, "0")
  | major :: minor :: point :: _ -> (major, minor, point)
  | [] -> failwith "Could not set major/minor/point version"

let number_of_cores () =
  let ncores = ref 0 in
  (* Compute number of cores, including hyper-threading, on a linux machine *)
  begin try
          FileString.iter_lines (fun line ->
            if OcpString.starts_with line ~prefix:"processor" then incr ncores
          ) "/proc/cpuinfo"
    with _ -> ()
  end;
  !ncores
