
type 'a t = {
    deep:bool;
    dft:[ `After | `Before ] option;
    filter:(bool -> string -> string -> bool);
    follow_links:bool;
    error:(exn -> string -> 'a -> unit);
}

let ok _dir _file _path = true
let no_error _exn _path _filename = ()

let create
    ?(deep=false)
    ?dft
    ?glob
    ?(filter=ok)
    ?(follow_links=false)
    ?(error=no_error)
    () =
  let filter = match glob with
    | None -> filter
    | Some glob ->
      let regexp = Re.Glob.glob ~anchored:true glob in
      let re = Re.compile regexp in
      fun is_dir file path ->
        (is_dir || Re.execp re file) && filter is_dir file path
  in
  {
    deep;
    dft;
    filter;
    follow_links;
    error;
  }
