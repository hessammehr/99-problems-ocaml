let rev lst =
  let rec revf lst acc = match lst with
      [] -> acc
    | x::xs -> revf xs (x::acc)
  in
  revf lst []

let main () =
  print_endline (string_of_bool (rev [1;2;3] = [3;2;1]));
  print_endline (string_of_bool (rev [1] = [1]));
  print_endline (string_of_bool (rev [] = []))

let _ = main ()
