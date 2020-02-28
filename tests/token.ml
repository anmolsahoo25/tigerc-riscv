(* token module *)
open Lexer.Regex

type op =
  | Plus
  | Minus

type token = 
  | If
  | El
  | Op of op
  | Id of string

let pprint_token = function _ -> ""
let token_regex = [
  (Concat (Symbol 'i', Symbol 'f'), fun _ -> If) ;
  (Concat (Symbol 'e', Symbol 'l'), fun _ -> El) ;
  (Choice (Symbol '+', Symbol '-'),
    function
      | "+" -> Op Plus
      | "-" -> Op Minus
      | _   -> assert false) ;
  (Repeat (Symbol 'i'), fun s -> Id s)
]
