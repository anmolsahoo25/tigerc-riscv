(* Implementation of regex matching *)

(* regex constructors *)
type regex =
  | Empty
  | Symbol of char
  | Concat of regex * regex
  | Choice of regex * regex
  | Repeat of regex

(* abstract types to track regex dfa states *)
type state 
type dfa

val compile_re   : regex -> (dfa * state)
val step_re      : (dfa * state) -> char -> (dfa * state) option
val is_accept    : state -> bool
val pprint_regex : regex -> string
