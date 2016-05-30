let duplicate lst =
  List.concat (List.map (fun x -> [x;x]) lst)

let example = ["a";"b";"c";"c";"d"]
let result = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]

let main () =
  print_endline (string_of_bool (duplicate example = result))

let _ = main ()
