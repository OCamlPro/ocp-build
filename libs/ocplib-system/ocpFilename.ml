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

include Filename

module Op = struct
  let (/) = concat
end

let get_extension filename =
  let filename = basename filename in
  try
    let n = String.rindex filename '.' in
    Some (OcpString.after filename n)
  with Not_found ->
    None

let suffix = get_extension
