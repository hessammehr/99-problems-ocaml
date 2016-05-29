let compress l =
  let rec compress_acc l acc last = match (l,acc) with
      ([],_) -> acc
    | (x::xs,[]) -> compress_acc xs (x::acc) x
    | (x::xs,acc) when x = last -> compress_acc xs acc last
    | (x::xs,acc) -> compress_acc xs (x :: acc) x in
  List.rev (compress_acc l [] "")

let example = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
let result = ["a"; "b"; "c"; "a"; "d"; "e"]

let main () =
  print_endline (string_of_bool (compress example = result))

let _ = main ()
