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



(** Extension of the stdlib Hashtbl module *)

open Hashtbl

(** Convert an hash-table into a list *)
val to_list : ('a, 'b) t -> ('a * 'b) list

(** Convert a list into an hash-table *)
val of_list : ('a * 'b) list -> ('a, 'b) t

(** Increments the integer value associated to a key *)
val incr : ('a, int) t ->  'a -> unit

(** Check whether a predicate holds on all key-value pairs of an
    hash-table *)
val for_all : ('a, 'b) t -> ('a -> 'b -> bool) -> bool

(** Check wether a predicate holds on at least one key-value pair of
    an hash-table *)
val exists : ('a, 'b) t -> ('a -> 'b -> bool) -> bool
