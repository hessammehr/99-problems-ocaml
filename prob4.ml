let rec length = function [] -> 0
                        | x::xs -> 1 + length xs

let print_int x = print_endline (string_of_int x)

let main () =
  print_int (length []);
  print_int (length [1]);
  print_int (length [1;2])

let _ = main ()
