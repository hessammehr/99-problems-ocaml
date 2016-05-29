let encode lst =
  let rec encode_acc l acc curr = match (l,curr) with
      ([],(0,_)) -> acc
    | ([],x) -> x::acc
    | (x::xs,(0,_)) -> encode_acc xs acc (1,x)
    | (x::xs,(n,y)) when x=y -> encode_acc xs acc (n+1,x)
    | (x::xs,y) -> encode_acc xs (y::acc) (1,x) in
  List.rev(encode_acc lst [] (0,""))

let example = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
let result = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]

let main () =
  print_endline (string_of_bool (encode example = result))

let _ = main ()
