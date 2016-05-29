let rec at n lst = match (n, lst) with
    (n, _) when n < 0 -> None
  | (n, []) -> None
  | (n, [x]) when n = 0 -> Some x
  | (n, x::xs) -> at (n-1) xs

let print_option = function None -> print_endline "None"
                          | Some num -> print_endline ("Some " ^ string_of_int num)

let main () =
  print_option (at 5 [1;2;3]);
  print_option (at (-2) [1;2;3]);
  print_option (at 1 [1;2;3]);
  print_option (at 0 []);
  print_option (at 0 [1])

let _ = main ()
