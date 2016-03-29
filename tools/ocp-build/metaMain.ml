(**************************************************************************)
(*                                                                        *)
(*                              OCamlPro TypeRex                          *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*      (GNU Public Licence version 3.0).                                 *)
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


open StringCompat
open MetaTypes

let string_of_stringo op = match op with
    None -> "--"
  | Some s -> Printf.sprintf "%S" s

let print_vars indent vars =
  StringMap.iter (fun s var ->
    Printf.printf "%s  %S -> { key = %S\n" indent s var.metavar_key;
    Printf.printf "%s          preds =\n" indent;
    List.iter (fun (s, bool) ->
      Printf.printf "%s               %S = %b\n" indent s bool
    ) var.metavar_preds;
    Printf.printf "%s          value = %s\n" indent (String.concat ","
        (List.map (fun s -> Printf.sprintf "%S" s) var.metavar_value));
  ) vars

let rec print indent m =
  Printf.printf "%s{ meta_version = %s\n" indent (string_of_stringo m.meta_version);
  Printf.printf "%s  meta_description = %s\n" indent (string_of_stringo m.meta_description);
  Printf.printf "%s  meta_exists_if =\n" indent;
  List.iter (fun s ->
    Printf.printf "%s   %S ?\n" indent s;
  )  m.meta_exists_if;
  Printf.printf "%s  meta_directory = %s\n" indent (string_of_stringo m.meta_directory);
  Printf.printf "%s  meta_preprocessor = %s\n" indent (string_of_stringo m.meta_preprocessor);
  Printf.printf "%s  meta_name = %s\n" indent (string_of_stringo m.meta_name);
  Printf.printf "%s  meta_linkopts = %s\n" indent (string_of_stringo m.meta_linkopts);
  Printf.printf "%s  meta_license = %s\n" indent (string_of_stringo m.meta_license);

  Printf.printf "%s  meta_requires = \n" indent;
  print_vars indent m.meta_requires;
  Printf.printf "%s  meta_archive =\n" indent;
  print_vars indent m.meta_archive;
  Printf.printf "%s  meta_error =\n" indent;
(*  print_vars indent m.meta_error; *)


  Printf.printf "%s  meta_package =\n" indent;
  List.iter (fun (s, meta) ->
    Printf.printf "%s    %S = \n" indent s;
    print indent meta;
  ) m.meta_package;
  Printf.printf "%s}\n%!" indent

let _ =
  for i = 1 to Array.length Sys.argv - 1 do
    let file = Sys.argv.(i) in
    try
      Printf.fprintf stderr "Parsing file %S\n%!" file;
      let (meta : MetaTypes.meta) = MetaParser.parse_file file in
      print "  " meta;
      let name = match meta.meta_name with
          None -> Filename.basename (Filename.dirname file)
        | Some name -> name in
      MetaFile.create_meta_file (Printf.sprintf "META.%s" name) meta
    with e ->
      Printf.fprintf stderr "MetaMain: Exception %S while parsing %S\n%!"
        (Printexc.to_string e) file
  done
