
let () =
  try
    let s = Sys.getenv "GITHUB_TOKEN" in
    Printf.eprintf "GITHUB_TOKEN found\n%!";
    if s <> "73e44596ab6168d96b499775b27e2fde4c93811a" then begin
      Printf.eprintf "GITHUB_TOKEN incorrect\n%!"
    end

  with Not_found ->
    Printf.eprintf "GITHUB_TOKEN not found\n%!"
