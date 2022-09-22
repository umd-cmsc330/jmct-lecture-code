type test = This
          | That;;

let test_to_string t =
  match t with
    | This -> "This"
    | That -> "That";;

print_string (test_to_string This)
