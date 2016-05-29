type 'a node =
  | One of 'a
  | Many of 'a node list

let example = [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ]
let result = [One "a"; One "b"; One "c"; One "d"; One "e"]

let rec flatten = function [] -> []
                         | (One x) :: xs -> (One x) :: (flatten xs)
                         | (Many x) :: xs -> List.concat[(flatten x); (flatten xs)]

let main () =
  print_endline (string_of_bool (flatten example = result))

let _ = main ()
