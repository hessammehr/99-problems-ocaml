let rec last lst = match lst with 
  | [] -> None
  | [i] -> Some i
  | i::is -> last is;;

let main () =
  print_endline (string_of_bool (last [1] = Some 1));
  print_endline (string_of_bool (last [] = None));
  print_endline (string_of_bool (last [1; 2; 3] = Some 3))

let _ = main ()
