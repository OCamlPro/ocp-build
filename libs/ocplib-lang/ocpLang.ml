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

(* Pervasives extension *)

include OcpPervasives

module List = struct
  include List
  include OcpList
end

module String = struct
  include String
  include OcpString
end

module Stream = struct
  include Stream
  include OcpStream
end

module Genlex = struct
  include Genlex
  include OcpGenlex
end

module Hashtbl = struct
  include Hashtbl
  include OcpHashtbl
end

module Digest = struct
  include Digest
  include OcpDigest
end
