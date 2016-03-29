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









open BuildEngineTypes

open BuildEngineContext



let byte_exe =
  match MinUnix.os_type with
     MinUnix.WINDOWS
   | MinUnix.CYGWIN -> ".byte.exe"
   | MinUnix.UNIX -> ".byte"

let asm_exe =
  match MinUnix.os_type with
     MinUnix.WINDOWS
   | MinUnix.CYGWIN -> ".asm.exe"
   | MinUnix.UNIX -> ".asm"

let add_dst_file b dst_dir filename =
  add_file b dst_dir (Filename.basename filename)

exception NoSuchFileInDir of string * string

let find_dst_file dst_dir filename =
  try
    find_file dst_dir (Filename.basename filename)
  with Not_found ->
    raise (NoSuchFileInDir (filename, dst_dir.dir_fullname))
