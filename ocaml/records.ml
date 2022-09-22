type person = { name : string; age : int; alias : string };;

let jose = { name = "Jose"; age = max_int; alias = "Josb" } in

let { alias = v } = jose in v;;
