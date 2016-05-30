let drop lst every =
  let rec drop_ac l acc n = match l with
      [] -> acc
    | x::xs when (n mod every) = every-1 -> drop_ac xs acc 0
    | x::xs -> drop_ac xs (x::acc) (n+1) in
  List.rev (drop_ac lst [] 0)

let example = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"]
let result = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]

let main () =
  print_endline (string_of_bool (drop example 3 = result))

let _ = main ()
