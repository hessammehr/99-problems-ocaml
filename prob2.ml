let rec last_two = function [] -> None
                          | [x] -> None
                          | [x;y] -> Some (x,y)
                          | x::xs -> last_two xs

let main () =
  print_endline (string_of_bool (last_two [] = None));
  print_endline (string_of_bool (last_two [1] = None));
  print_endline (string_of_bool (last_two [1;2] = Some (1,2)));
  print_endline (string_of_bool (last_two [1;2;3;4] = Some (3,4)))

let _ = main ()
