let palindrome str =
  let rec get_char_list =
    function "" -> []
           | str -> (String.get str 0) ::
                    (get_char_list (String.sub str 1 (String.length str - 1))) in
  let lst = get_char_list str in
  (List.rev lst) = lst

let main () =
  print_endline (string_of_bool (palindrome "mayamoodybabydoomayam"));
  print_endline (string_of_bool (palindrome "madamimadam"));
  print_endline (string_of_bool (palindrome "nolemonnomelon"));
  print_endline (string_of_bool (palindrome "xolemonnomelon"))

let _ = main ()
