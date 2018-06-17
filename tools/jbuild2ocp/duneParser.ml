open StringCompat

module Cst = Parsexp.Cst

type sexp =
  | Atom of string
  | List of sexp list

let rec remove_comments = function
  | Cst.Sexp (Cst.Atom { atom }) -> Some (Atom atom)
  | Cst.Sexp (Cst.List { elements }) ->
     Some (List (remove_comments_list elements))
  | Cst.Comment _ -> None

and remove_comments_list elements =
  List.fold_right (fun ele acc ->
      match remove_comments ele with
      | None -> acc
      | Some ele -> ele :: acc)
                 elements []

let read filename =
  let input = FileString.read_file filename in
  let sexps = Parsexp.Many_cst.parse_string_exn input in
  remove_comments_list sexps


let rec string_of_sexp =
  function | Atom s -> Printf.sprintf "%S" s
           | List list ->
              let list = List.map string_of_sexp list in
              Printf.sprintf "(%s)" (String.concat " " list)
