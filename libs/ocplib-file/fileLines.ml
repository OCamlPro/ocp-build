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

let write_file file lines =
    FileString.write_lines file (Array.of_list lines)
let read_file file =
  List.rev (FileString.read_lines_to_revlist file)
let iter = FileString.iter_lines
let iteri = FileString.iteri_lines
let sub file pos len =
  Array.to_list (FileString.read_sublines file pos len)
