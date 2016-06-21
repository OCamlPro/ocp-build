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


open BuildOCPTypes

let create_package name ptype dirname_t =
  let file_t = File.add_basename dirname_t (name ^ ".ocp") in

  let map = ref StringMap.empty in
  let files = Dir.list dirname_t in
  List.iter (fun file ->
    try
      let modfile =
        let modfile = Bytes.of_string file in
        modfile.[0] <- Char.uppercase (Bytes.get modfile 0);
        Bytes.to_string modfile
      in
      let basename, ext = FileString.cut_at_last_extension modfile in
      let modfile = basename ^ "." ^ ext in
      map := StringMap.add modfile (file, basename, ext) !map
    with Not_found -> ()
  ) files;

  let map = !map in
  let files = ref [] in
  StringMap.iter (fun _modfile (file, basename, ext) ->
    match ext with
        "ml" ->
          if not (StringMap.mem (basename ^ ".mll") map) &&
            not (StringMap.mem (basename ^ ".mly") map) then
            files := file :: !files
      | "mli" ->
        if not (StringMap.mem (basename ^ ".mly") map) &&
          not (StringMap.mem (basename ^ ".ml") map) then
          files := file :: !files
      | "mly"
      | "mll"
      | "c" ->
        files := file :: !files
      | _ -> ()
  ) map;
  let source_files = !files in

  let oc = File.open_out file_t in
  Printf.fprintf oc "begin %s \"%s\"\n"
    (match ptype with
    | ProgramPackage -> "program"
    | ObjectsPackage -> "objects"
    | LibraryPackage -> "library"
    | SyntaxPackage -> "syntax"
    | TestPackage -> "test"
    | RulesPackage -> "rules"
    (*      | ProjectToplevel -> "toplevel" *)
    )
    name;
  Printf.fprintf oc "   sort = true\n";
  Printf.fprintf oc "   files = [ %s ]\n" (match source_files with
      [] -> ""
    | _ -> "\"" ^ String.concat "\" \"" source_files ^ "\"");
  Printf.fprintf oc "   requires = [ ]\n";
  Printf.fprintf oc "end\n";
  close_out oc
