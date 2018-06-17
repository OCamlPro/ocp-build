

module TYPES : sig

  type sexp =
    | Atom of string
    | List of sexp list

end

val read : string -> TYPES.sexp list
val string_of_sexp : TYPES.sexp -> string
