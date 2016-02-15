
module Bytes = Bytes
module Buffer = Buffer

module String = struct
  include String
  let set = Bytes.set
end

module StringSet = Set.Make(String)

module StringMap = struct
  module M = Map.Make(String)
  include M
  let of_list list =
    let map = ref empty in
    List.iter (fun (x,y) -> map := add x y !map) list;
    !map

  let to_list map =
    let list = ref [] in
    iter (fun x y -> list := (x,y) :: !list) map;
    List.rev !list

  let to_list_of_keys map =
    let list = ref [] in
    iter (fun x y -> list := x :: !list) map;
    List.rev !list
end
