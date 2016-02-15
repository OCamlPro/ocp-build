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


open Unix

(** [expand env s] expands the string [s] in the environment [env] *)
val expand_in : string array -> string -> string

(** [in_dir dir fn] executes [fn] in the directory [dir] *)
val in_dir : string -> (unit -> 'a) -> 'a

(** Return all the directories in the current directory *)
val directories : unit -> string list

(** Return all the files (ie. non-directories) in the current directory *)
val files : unit -> string list

(** Return all the sub-files *)
val tree : unit -> string list

(** Remove a file and do not fail if it not exists *)
val safe_unlink : string -> unit

(** Create a directory and do not fail it is alreadly exists *)
val safe_mkdir : string -> file_perm -> unit

(** Create recursively a directory and all its parents if they not
    exist *)
val safe_rec_mkdir : string -> file_perm -> unit

(** Remove recursively a directory and all the files it contain *)
val safe_rec_rmdir : string -> unit

(*
(** Must call this before spawning any threads! *)
val daemonize : unit -> unit
*)

(** Returns the standard output of [fn ()] exection, as a string *)
val string_of_stdout : (unit -> unit) -> string

(** Return the modification time of a file *)
val modtime : string -> float
