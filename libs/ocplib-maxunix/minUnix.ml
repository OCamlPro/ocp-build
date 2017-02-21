include Unix


type os_type = WINDOWS | CYGWIN | UNIX

let os_type =
  match String.lowercase Sys.os_type with
      "win32" -> WINDOWS
    | "cygwin" -> CYGWIN
    | "unix" -> UNIX
    | _ -> assert false

(* added by OCamlPro *)
external strftime : string -> tm -> string = "minUnix_strftime"
