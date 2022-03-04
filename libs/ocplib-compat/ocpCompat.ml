(**************************************************************************)
(*                                                                        *)
(*   Typerex Libraries                                                    *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


#if OCAML_VERSION < "4.04"
module String404 = struct
  include String
  let split_on_char c s =
    let len = String.length s in
    let rec iter pos to_rev =
      if pos = len then List.rev ("" :: to_rev) else
        match try
            Some ( String.index_from s pos c )
          with Not_found -> None
        with
          Some pos2 ->
          if pos2 = pos then iter (pos+1) ("" :: to_rev) else
            iter (pos2+1) ((String.sub s pos (pos2-pos)) :: to_rev)
        | None -> List.rev ( String.sub s pos (len-pos) :: to_rev )
    in
    iter 0 []
end
#else
module String404 = String
#endif

#if OCAML_VERSION < "4.03"
module String403 = struct
  include String404
  let lowercase_ascii = lowercase
  let uppercase_ascii = uppercase
  let capitalize_ascii = capitalize
end

module Char = struct
  include Char
  let uppercase_ascii = uppercase
  let lowercase_ascii = lowercase
end
#else
module String403 = struct
  include String404
  let lowercase = lowercase_ascii
  let uppercase = uppercase_ascii
  let capitalize = capitalize_ascii
end
module Char = struct
  include Char
  let uppercase = uppercase_ascii
  let lowercase = lowercase_ascii
end
#endif

#if OCAML_VERSION < "4.02"

type bytes = string

module Bytes = struct
  include String403
  let to_string t = String.copy t
  let of_string t = String.copy t
  let unsafe_to_string t = t
  let unsafe_of_string t = t
  let sub_string = String.sub
  let blit_string = String.blit
 end

module Buffer = struct
  include Buffer
  let to_bytes b = contents b
  let add_subbytes = add_substring
end

module Marshal = struct
  include Marshal
  let from_bytes = from_string
end

module String = String403

let print_bytes = print_string
let prerr_bytes = prerr_string
let output_bytes = output_string
let output_substring = output
let really_input_string ic len =
  let s = String.create len in
  really_input ic s 0 len;
  s

#else

module Bytes = Bytes
module Buffer = Buffer

module String = struct
  include String403
  let set = Bytes.set
end

#endif

module IntSet : sig

  include Set.S with type elt = int

end = struct

  module Set = Set.Make(struct type t = int
                               let compare (x:int) y = compare x y
                        end)

  include Set
end


module StringSet = struct
  module M = Set.Make(String)
  include M

  let of_list list =
    let map = ref empty in
    List.iter (fun x -> map := add x !map) list;
    !map

  let to_list set =
    let list = ref [] in
    iter (fun e -> list := e :: !list) set;
    List.rev !list

end


module IntMap : sig

  include Map.S with type key = int

  val to_list: 'a t -> (int * 'a) list
  val to_list1: 'a t -> int list
  val to_list2: 'a t -> 'a list

  exception MinElt
  val min_elt: 'a t -> (key * 'a) option

end = struct
  module Map = Map.Make(struct type t = int
                               let compare (x:int) y = compare x y
                        end)

  include Map

  let to_list map =
    let list = ref [] in
    iter (fun x y -> list := (x,y) :: !list) map;
    List.rev !list

  let to_list1 map =
    let list = ref [] in
    iter (fun x _y -> list := x :: !list) map;
    List.rev !list

  let to_list2 map =
    let list = ref [] in
    iter (fun _x y -> list := y :: !list) map;
    List.rev !list

  exception MinElt
  let exn_MinElt = MinElt

  let min_elt map =
    let x = ref None in
    try
      iter (fun key v -> x := Some (key, v); raise exn_MinElt) map;
      None
    with MinElt -> !x

end

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
    iter (fun x _y -> list := x :: !list) map;
    List.rev !list
end
