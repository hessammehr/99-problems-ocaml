type 'a rle =
  | One of 'a
  | Many of int * 'a

let decode seq =
  let rec decode_token x = match x with
      One y -> [y]
    | Many (2,y) -> y::decode_token (One y) (* For correctness *)
    | Many (n,y) -> y::decode_token (Many (n-1,y)) in
  List.concat (List.map decode_token seq)

let example = [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")]
let result = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]

let main () =
  print_endline (string_of_bool (decode example = result))

let _ = main ()
