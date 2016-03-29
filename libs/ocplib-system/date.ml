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


open MinUnix

type iso8601 = string

let iso8601_tm t =
  Printf.sprintf
    "%04d%02d%02dT%02d:%02d:%02dZ"
    (1900 + t.tm_year)
    (1 + t.tm_mon)
    t.tm_mday
    t.tm_hour
    t.tm_min
    t.tm_sec

let iso8601 () =
  iso8601_tm (gmtime (time ()))

let string_of_iso8601 x = x

type timestamp = string

let timestamp_tm t =
  Printf.sprintf
    "%04d%02d%02d%02d%02d%02d"
    (1900 + t.tm_year)
    (1 + t.tm_mon)
    t.tm_mday
    t.tm_hour
    t.tm_min
    t.tm_sec

let timestamp () =
  timestamp_tm (gmtime (time ()))

let string_of_timestamp x = x
