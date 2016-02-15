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



type t =
    Inode of int * int * float
  | Digest of Digest.t

      (*
let use_digests = ref false

let zero = Inode (0,0, 0.)

let to_string mtime =
  match mtime with
      Inode (dev, ino, mtime) -> Printf.sprintf "%d.%d.%.0f" dev ino mtime
    | Digest sha1 -> OcpDigest.to_hex sha1

let compute filename =
  if !use_digests then
    Digest (Digest.file filename)
  else
    let st = MinUnix.lstat filename in
    Inode (st.MinUnix.st_dev, st.MinUnix.st_ino, st.MinUnix.st_mtime)

let use_digests bool = use_digests := bool
      *)
val zero : t
val to_string : t -> string
val compute : string -> t
val use_digests : bool -> unit
