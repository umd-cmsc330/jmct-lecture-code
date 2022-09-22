let addf x y = (int_of_float x) + y;;

(* Apply a function to each element _independently_, result is a new list *)
let rec map f xs =
  match xs with
  | []        -> []
  | (y::rest) -> f y :: map f rest;;


let rec fold_r f x xs = 
  match xs with
  | []        -> x
  | (y::rest) -> f y (fold_r f x rest)













(* map : ('z -> 'a) -> 'z list -> 'a list

f    : 'z -> 'a
xs   : 'z list
y    : 'z
rest : 'z list
*)

