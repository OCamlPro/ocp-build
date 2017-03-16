
open Lwt.Infix

let f xhr u response =
  xhr##.onreadystatechange := Js.wrap_callback (fun _ ->
    Lwt.wakeup u response) ;
