(* convert string to character list *)
(* fold to chaacter list *)
(* function that splits on whitespace *)
(* lex: string -> token list *)
type token = Plus 
            | Num of int 
            | Minus 
            | Div 
            | Square
            | Mult;; 


let re_plus = Str.regexp "+";;
let re_mnus = Str.regexp "-";;
let re_mult = Str.regexp "*";;
let re_div = Str.regexp "/";;
let re_sq = Str.regexp "?";;
let re_num = Str.regexp "[0-9]";;

let rec lex str = 
  if str = "" then []
  else if Str.string_match re_num str 0 then
    Num(int_of_string (String.sub str 0 1))::
    (lex (String.sub str 1 ((String.length str) - 1)))
  else if Str.string_match re_plus str 0 then
    Plus::(lex (String.sub str 1 ((String.length str) - 1)))
  else if Str.string_match re_mnus str 0 then
    Minus::(lex (String.sub str 1 ((String.length str) - 1)))
  else if Str.string_match re_mult str 0 then
    Mult::(lex (String.sub str 1 ((String.length str) - 1)))
  else if Str.string_match re_div str 0 then
    Div::(lex (String.sub str 1 ((String.length str) - 1)))
  else if Str.string_match re_sq str 0 then
    Square::(lex (String.sub str 1 ((String.length str) - 1)))
  else 
    (lex (String.sub str 1 ((String.length str) - 1)));;

(* parse: token list -> AST *)

(*type ast = OP of string * ast * ast| Num of int *)

type op = PLUS | MINUS | MULT | DIV|SQ;;
type ast = OP of op * ast * ast | Int of int | Empty;;

let rec parse toks = let ast,rest= parse_E toks in 
if rest = [] then ast
else failwith "Something went wrong"

and parse_E toks = match toks with 
  [] -> (Empty,[])
  |Plus::t -> 
              let (mid,rtok) = parse_E t in
              let (right,rest) = parse_E rtok in
              (OP(PLUS,mid,right),rest)
  |Minus::t -> 
              let (mid,rtok) = parse_E t in
              let (right,rest) = parse_E rtok in
              (OP(MINUS,mid,right),rest)
  |Mult::t -> 
              let (mid,rtok) = parse_E t in
              let (right,rest) = parse_E rtok in
              (OP(MULT,mid,right),rest)
  |Div::t -> 
              let (mid,rtok) = parse_E t in
              let (right,rest) = parse_E rtok in
              (OP(DIV,mid,right),rest)
  |Square::t -> 
              let (mid,rtok) = parse_E t in
              (OP(SQ,mid,Empty),rtok)
  |Num(x)::t -> (Int(x),t);;

  (* parse_help: token list -> (ast, tokens_left) *)
(*
type op = PLUS | MINUS | MULT | DIV;;
type ast = OP of op * ast * ast | Int of int | Empty;;
*)

let rec eval ast = match ast with
Empty -> failwith "Something went wrong"
|Int(x) -> x
|OP(PLUS,m,r) -> (eval m) + (eval r)
|OP(MINUS,m,r)-> (eval m) - (eval r)
|OP(MULT,m,r)-> (eval m) * (eval r)
|OP(DIV,m,r)-> (eval m) / (eval r)
|OP(SQ,m,Empty)-> (eval m) * (eval m);;

