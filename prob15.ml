let replicate lst n =
  let rec repeat n x = match n with
      0 -> []
    | n -> x::(repeat (n-1) x) in
  List.concat (List.map (repeat n) lst)

let example = ["a";"b";"c"]
let result = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]

let main () =
  print_endline (string_of_bool (replicate example 3 = result))

let _ = main ()
