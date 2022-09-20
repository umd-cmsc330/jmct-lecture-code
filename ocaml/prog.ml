type int_list = Empty
              | Cons of int * int_list;;

let rec list_max xs =
  match xs with
  | Empty            -> 0
  | Cons (v, rest) -> let rmax = list_max rest in if rmax > v then rmax else v;;
