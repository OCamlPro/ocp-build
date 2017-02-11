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

module Op : sig
  val (/) : string -> string -> string
end


(** [get_extension "foo/bar.t"] returns [Some "t"] *)
val get_extension : string -> string option

(** Same as {get_extension} but we keep it for backward
    compatibility *)
val suffix : string -> string option
