(**************************************************************************)
(*                                                                        *)
(*                              OCamlPro TypeRex                          *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the LGPL v2.1 with       *)
(*   the special exception on linking described in the file LICENSE.      *)
(*      (GNU Lesser General Public Licence version 2.1)                   *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)

type bytes = string

module String = struct
  include String
  let lowercase_ascii = lowercase
  let uppercase_ascii = uppercase
  let capitalize_ascii = capitalize
end

module Char = struct
  include Char
  let uppercase_ascii = uppercase
  let lowercase_ascii = lowercase
end

module Bytes = struct
  include String
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

let print_bytes = print_string
let prerr_bytes = prerr_string
let output_bytes = output_string
let output_substring = output
let really_input_string ic len =
  let s = String.create len in
  really_input ic s 0 len;
  s

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
