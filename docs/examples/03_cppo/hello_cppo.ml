

let () =
#ifdef TOTO
  print_endline "TODO is defined"
#else
  print_endline "TODO is not defined"
#endif
