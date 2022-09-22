type int_list = Empty
              | Cons of int * int_list;;

type 'a listy = End
              | Elem of 'a * 'a listy;;

let rec list_max xs =
  match xs with
  | Empty          -> None
  | Cons (v, rest) -> (match list_max rest with
                       | None      -> Some v
                       | Some rmax -> Some (if rmax > v then rmax else v));;
