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

(* open BuildBase *)
(* open Stdlib2 *)

(*
      1 IDENT[license]
      1 IDENT[descriptions]
      2 IDENT[predicates]
      3 IDENT[plugin_name]
      3 IDENT[plugin_synopsis]
      3 IDENT[plugin_system]
      5 IDENT[type_of_threads]
      6 IDENT[preprocessor]
     16 IDENT[linkopts]
     27 IDENT[name]
     49 IDENT[browse_interfaces]
     75 IDENT[directory]
    180 IDENT[exists_if]
    457 IDENT[requires]
    464 IDENT[version]
    474 IDENT[description]
*)

(*
     18 IDENT[error]
     40 IDENT[requires]
   1313 IDENT[archive]
*)

type meta = {
  mutable meta_version : string option;
  mutable meta_description : string option;
  mutable meta_exists_if : string list;
  mutable meta_directory : string option;
  mutable meta_preprocessor : string option;
  mutable meta_name : string option;
  mutable meta_linkopts : string option;
  mutable meta_license : string option;
(*  mutable meta_browse_interfaces : string list; *)

  mutable meta_requires : string list var StringMap.t;
  mutable meta_archive : string list var StringMap.t;
  mutable meta_error : string var StringMap.t;

  mutable meta_package : (string * meta) list;
}

and 'a var = {
  metavar_key : string;
  metavar_preds : (string * bool) list;
  mutable metavar_value : 'a;
}
