let pack lst =
  let rec pack_acc l acc cur = match (l,cur) with
      ([],[]) -> acc
    | ([],cur) -> cur::acc
    | (x::xs,[]) -> pack_acc xs acc [x]
    | (x::xs,y::ys) when x=y -> pack_acc xs acc (x::cur)
    | (x::xs,_) -> pack_acc xs (cur::acc) [x] in
  List.rev (pack_acc lst [] [])

let example = ["";"a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]
let result = [[""];["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
              ["e"; "e"; "e"; "e"]]

let main () =
  print_endline (string_of_bool (pack example = result))

let _ = main ()
