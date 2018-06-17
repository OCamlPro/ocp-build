
type sexp =
  | Atom of string
  | List of sexp list

val read : string -> sexp list
val string_of_sexp : sexp -> string
