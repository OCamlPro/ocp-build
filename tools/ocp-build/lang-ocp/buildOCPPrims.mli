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

open OcpCompat
open BuildValue.TYPES
open BuildOCPTree

module Init(S: sig

    val filesubst : (string * env list) BuildSubst.t

  end) : sig

  val primitives : ((env list -> env -> plist) * string list) StringMap.t ref
  val primitives_help : unit -> string list StringMap.t

end
