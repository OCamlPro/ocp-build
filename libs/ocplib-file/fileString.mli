(**************************************************************************)
(*                                                                        *)
(*                              OCamlPro TypeRex                          *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the LGPL v2.1 with       *)
(*   the special exception on linking described in the file LICENSE.      *)
(*      (GNU Lesser General Public Licence version 2.1)                   *)
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

(* This module implements operations on filenames represented as strings,
   as in the Filename module of the Standard Library. *)

open StringCompat

include (FileSig.FILE_OPERATIONS with type t := string)

(* [iter_dir f dirname] read directory [dirname] and calls [f] on
   all the basenames of the files. *)
val iter_dir : (string -> unit) -> string -> unit

(* [iter_files ?recursive f dirname] reads, maybe recursively,
   directory [dirname] and calls [f] on all the files. The argument
   of [f] for a given file is the relative filename of [file] from
   [dirname]. *)
val iter_files : ?recursive:bool -> (string -> unit) -> string -> unit

(** [cut_at_last_extension file] returns a pair
    [before_ext,extension], where [extension] is the last extension,
    converted to lowercase, and without the initial dot, and
    [before_ext] everything before the last dot. *)
val cut_at_last_extension : string -> string * string

(* [last_extension extensions] returns the last extension (equivalent
   to [OcpList.last] without exceptions) *)
val last_extension : string list -> string option

(* [extensions_of_basename basename] returns the list of extensions of
   the file. The argument must only contain the basename of the file,
   otherwise the dot may belong to a parent directory. *)
val extensions_of_basename : string -> string list

module Op : sig

  val (//) : string -> string -> string

  end
