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


type ansi_sequences = {
  mutable esc_ansi : bool;
  mutable esc_bold : string;
  mutable esc_black_text : string;
  mutable esc_red_text : string;
  mutable esc_green_text : string;
  mutable esc_yellow_text : string;
  mutable esc_blue_text : string;
  mutable esc_magenta_text : string;
  mutable esc_cyan_text : string;
  mutable esc_white_text : string;
  mutable esc_end : string;
  mutable esc_linefeed : string;
  mutable esc_killline : string;
  mutable esc_columns : int;
}

val term : ansi_sequences
val set_ansi_term : bool -> unit
val term_bold : string -> string
val term_escape : string -> string



