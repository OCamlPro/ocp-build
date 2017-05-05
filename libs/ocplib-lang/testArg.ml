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

let () =

  let init_args = [
      OcpArg.string "--root1" (fun s ->
                      print_string s; print_newline ())
                    "ROOTDIR Print root dir";
      OcpArg.string "--root2" (fun s ->
                      print_string s; print_newline ())
                    "{ROOT DIR} Print root ROOT and dir DIR";
      OcpArg.string "--root3" ~alias:["-r"] (fun s ->
                      print_string s; print_newline ())
                    "{ROOT DIR} Print root ROOT and dir DIR and a very very long description";
      OcpArg.string "--root4" (fun s ->
                      print_string s; print_newline ())
                    "{ROOT DIR AND MORE ARGS} Print root ROOT and dir DIR";
    ] in

  let group = [
      "init", OcpArg.command ~args:init_args
                             ~action:(fun list ->
                               Printf.printf "INIT ACTION: %s\n%!"
                                             (String.concat "+" list)
                             )
                             "Init example";
    ] in

  let cmd = OcpArg.command ~group
                           "Test OcpArg module" in


  let version = "0.1" in
  let build = "2017-05-03" in
  OcpArg.run  ~version ~build cmd
