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

open OcpCompat

module type CONTENT_OPERATIONS = sig

  type in_file
  type out_file

(*

   Converting file contents to string, back and forth.

*)


  (* [read_file file] returns the content of [file] *)
  val read_file : in_file -> string
  (* [string_of_file] is an alias for [read_file] *)
  val string_of_file : in_file -> string

  (* [write_file file content] creates file [file] with content [content] *)
  val write_file : out_file -> string -> unit
  (* [file_of_string] is an alias for [write_file] *)
  val file_of_string : out_file -> string -> unit

  (* [read_subfile file pos len] returns a string containing [len] bytes
     read from file [file] at pos [pos]. Raises [End_of_file] if the file
     is too short.  *)
  val read_subfile : in_file -> int -> int -> string
  (* [string_of_subfile] is an alias for [read_subfile] *)
  val string_of_subfile : in_file -> int -> int -> string

(*

   Converting file contents to lines, back and forth.

*)

  (* [read_lines file] returns the content of [file] as an array of
     lines *)
  val read_lines :  in_file -> string array
  val read_lines_to_revlist :  in_file -> string list

  (* [lines_of_file] is an alias for [read_lines] *)
  val lines_of_file : in_file -> string array

  (* [write_lines file lines] creates the file [file] from an array of
     lines, using [FileChannel.output_line]. *)
  val write_lines :  out_file -> string array -> unit
  (* [file_of_lines] is an alias for [write_lines] *)
  val file_of_lines : out_file -> string array -> unit

  (* [read_sublines file pos len] returns at most [len] lines of the
     file [file], starting at line [pos]. It differs from [read_subfile]
     in that it will not raise any exception if the file is too
     short. *)
  val read_sublines : in_file -> int -> int -> string array



  (*

   Iterators on chunks and lines.

  *)


  (* [iter_blocks f file] reads the content of file [file], and calls
     [f] on each chunk. Chunks have a maximal size of 32768. *)
  val iter_blocks : (bytes -> int -> int -> unit) -> in_file -> unit

(* [iter_lines f file] calls [f line] on all the line [line] of
   the file [file]. *)
  val iter_lines : (string -> unit) -> in_file -> unit

  (* [iteri_lines f file] calls [f line_num line] on every line [line]
     of the file [file], with [line_num] the line number, starting with
     line 0. *)
  val iteri_lines : (int -> string -> unit) -> in_file -> unit


  (* [copy_file src dst] creates a file [dst] with the content of [src] *)
  val copy_file : in_file -> out_file -> unit


end

module type FILE_OPERATIONS = sig

  type t

  include (CONTENT_OPERATIONS with type in_file := t and type out_file := t)

(*

Operations on filenames

*)

  val concat : t -> t -> t
  val is_absolute : t -> bool
  val is_relative : t -> bool
  val is_implicit : t -> bool

  val add_suffix : t -> string -> t
  val check_suffix : t -> string -> bool

  (* [extensions file] returns the list of extensions of the file *)
  val extensions : t -> string list

  val basename : t -> string
  val dirname : t -> t
  val add_basename : t -> string -> t
  val add_basenames : t -> string list -> t

  val chop_extension : t -> t
  val last_extension : t -> string option

  val current_dir_name : t

(*

  Standard operations on files

*)


  val open_in : t -> in_channel
  val open_out : t -> out_channel
  val open_in_bin : t -> in_channel
  val open_out_bin : t -> out_channel
  val open_fd :
    t -> MinUnix.open_flag list -> MinUnix.file_perm -> MinUnix.file_descr
  val temp_file : t -> string -> t


  val exists : t -> bool
  val getcwd : unit -> t
  val size : t -> int
  val is_directory : t -> bool
  val is_link : t -> bool

  val remove : t -> unit
  val rename : t -> t -> unit

  val stat : t -> MinUnix.stats
  val lstat : t -> MinUnix.stats



(*

Copying files

*)


  (* [safe_mkdir dirname] creates a directory [dirname], potentially
     creating any parent directory. A [~mode] argument can be
     provided, otherwise it is assumed to be 0o755. [safe_mkdir] can
     fail for wrong permissions, or if a directory name is already
     used by another kind of files.*)
  val safe_mkdir : ?mode:int -> t -> unit

  (* [copy_rec src dst] creates [dst] as a copy of [src], even
     if [src] is a directory. *)
  val copy_rec : t -> t -> unit

  (* [uncopy_rec src dst] removes from [dst] everything that has the
     same name as in [src]. Can be seen as the inverse operation of
     [copy_rec]. *)
  val uncopy_rec : t -> t -> unit


end
