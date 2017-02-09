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

include module type of OcpPervasives

module List : sig
  include module type of List
  include module type of OcpList
end

module String : sig
  include module type of String
  include module type of OcpString
end

module Stream : sig
  include module type of Stream
  include module type of OcpStream
end

module Genlex : sig
  include module type of Genlex
  include module type of OcpGenlex
end

module Hashtbl : sig
  include module type of Hashtbl
  include module type of OcpHashtbl
end

module Digest : sig
  include module type of Digest
  include module type of OcpDigest
end
