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


open StringCompat

(* [copy_file src dst] creates a file [dst] with the content of [src] *)
val copy_file : string -> string -> unit

(* [iter_blocks f file] reads the content of file [file], and calls
   [f] on each chunk. Chunks have a maximal size of 32768. *)
val iter_blocks : (bytes -> int -> int -> unit) -> string -> unit

(* [safe_mkdir dirname] creates a directory [dirname], potentially
   creating any parent directory. A [~mode] argument can be
   provided, otherwise it is assumed to be 0o755. [safe_mkdir] can
   fail for wrong permissions, or if a directory name is already
   used by another kind of files.*)
val safe_mkdir : ?mode:int -> string -> unit

(* [copy_rec src dst] creates [dst] as a copy of [src], even
   if [src] is a directory. *)
val copy_rec : string -> string -> unit

(* [uncopy_rec src dst] removes from [dst] everything that has the
   same name as in [src]. Can be seen as the inverse operation of
   [copy_rec]. *)
val uncopy_rec : string -> string -> unit

(* [iter_dir f dirname] read directory [dirname] and calls [f] on
   all the basenames of the files. *)
val iter_dir : (string -> unit) -> string -> unit

(* [iter_files ?recursive f dirname] reads, maybe recursively,
   directory [dirname] and calls [f] on all the files. The argument
   of [f] for a given file is the relative filename of [file] from
   [dirname]. *)
val iter_files : ?recursive:bool -> (string -> unit) -> string -> unit


val output_line : out_channel -> string -> unit

val file_of_lines : unit
val lines_of_file : unit
