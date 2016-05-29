type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode lst =
  let rec encode_acc l acc cur = match (l,cur) with
      ([],_) -> cur::acc
    | (x::xs,One y) when x=y -> encode_acc xs acc (Many (2,x))
    | (x::xs,(Many (n,y))) when x=y -> encode_acc xs acc (Many ((n+1),x))
    | (x::xs,_) -> encode_acc xs (cur::acc) (One x) in
  match lst with
    [] -> []
  | x::xs -> List.rev (encode_acc xs [] (One x))

let example = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
let result = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
              Many (4, "e")]

let main () =
  print_endline (string_of_bool (encode example = result))

let _ = main ()
