let _ =
  let rec iter n =
    let min_n = n/4 + 1 in
    let max_n = n/2 in
    let space_fast = 15 + 5 * n / 2 in
    let space_std_min = 8 + 2 * n in
    let space_std_max = 4 + 3 * n in
      Printf.printf "%d %d %d %d\n" min_n n space_fast space_std_min;
      Printf.printf "%d %d %d %d\n" max_n n space_fast space_std_max;
      let n = 2*n in
	if n < 100_000_000 then
	  iter n
  in
    iter 10

