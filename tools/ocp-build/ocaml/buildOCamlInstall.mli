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

module TYPES : sig

type install_where = {
  install_destdir : string option;

  install_libdirs : string list;
  install_bindir : string;
  install_datadir : string option;

  install_ocamlfind : string list;
  install_ocamllib : string;
}

type install_what = {
  install_byte_bin : bool;
  install_asm_bin : bool;
  install_byte_lib : bool;
  install_asm_lib : bool;
}
end
open TYPES

val install :
  install_where ->
  install_what ->
  BuildTypes.package_info -> string -> unit

val find_installdir :
  install_where ->
  BuildOCamlTypes.ocaml_package -> string option


val install_where :
           BuildOptions.config_input ->
           BuildOCamlConfig.TYPES.config_output -> install_where
val install_what : unit -> install_what
