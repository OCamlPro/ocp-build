(**************************************************************************)
(*                                                                        *)
(*   Typerex Tools                                                        *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU General Public License version 3 described in the file       *)
(*   LICENSE.                                                             *)
(*                                                                        *)
(**************************************************************************)

val create : MetaTypes.t option -> MetaTypes.t
val get_variable :
    MetaTypes.t -> string -> MetaTypes.variable

val parse_file : string -> MetaTypes.t
val name_of_META : string (* full filename *) -> string
