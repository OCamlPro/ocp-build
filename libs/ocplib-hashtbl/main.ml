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

let initial = ref 10000000
let total = ref 10_000_000
let float_key = ref false
let float_value = ref false

let time s f =
  let t0 = Unix.gettimeofday () in
  f ();
  let t1 = Unix.gettimeofday () in
  Printf.printf "%s: %.3f seconds\n%!" s (t1 -. t0)

module MakeTest(M : sig

  type ('a,'b) t
  val create : int -> ('a,'b) t
  val add : ('a,'b) t -> 'a -> 'b -> unit
  val find : ('a,'b) t -> 'a -> 'b

end) = struct

  let test to_key to_value =
    let m = M.create !initial in
    time "puts" (fun _ ->
      for n=1 to !total do
	M.add m (to_key n) (to_value n)
      done);
    time "gets" (fun _ ->
      for n = 1 to !total do
	assert (M.find m (to_key n) = to_value n)
      done)

  let int_of_int n = n

  let test () =
  match !float_key, !float_value with
    | false, false -> test int_of_int int_of_int
    | true, false -> test float_of_int int_of_int
    | false, true -> test int_of_int float_of_int
    | true, true -> test float_of_int float_of_int


end

module HashtblTest = MakeTest(StdHashtbl)
module FastHashtblTest = MakeTest(FastHashtbl)



let test_fast = ref true

let arg_list =    [
  "-std", Arg.Clear test_fast, " : test stdlib version";
  "-fast", Arg.Set test_fast, " : test fast version";
  "-init", Arg.Int ((:=) initial), " : set initial table size";
  "-total", Arg.Int ((:=) total), " : set number of elements to add";
  "-float_key", Arg.Set float_key, " : keys are floats";
  "-float_value", Arg.Set float_value, " : values are floats";
]

let arg_usage = " : compare performances of hashtbls"

let _ =
  Arg.parse arg_list
    (fun _ -> Arg.usage arg_list arg_usage)
    arg_usage;
(*
  if !test_fast then
    FastHashtblTest.test ()
  else
    HashtblTest.test ()
*)
  ()

let flip_bool name v f =
  Printf.printf "With %s = true\n%!" name;
  v := true;
  f ();
  Printf.printf "With %s = false\n%!" name;
  v := false;
  f ()

let flip_int name ref values f =
  List.iter (fun v ->
    Printf.printf "With %s = %d\n%!" name v;
    ref := v;
    f ();
  ) values

let _ =
  flip_int "init" initial [ 100; 100_000; 10_000_000 ] (fun _ ->
    flip_bool "float_key" float_key  (fun _ ->
      flip_bool "float_value" float_value (fun _ ->
	flip_bool "fast version" test_fast (fun _ ->
	  if !test_fast then
	    FastHashtblTest.test ()
	  else
	    HashtblTest.test ())
	)))
