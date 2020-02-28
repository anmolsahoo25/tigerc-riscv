(* Functor to create a lexer *)

(* Lexer tokens *)
module type TokenSig =
  sig
    type token
    val  pprint_token : token -> string
    val  token_regex  : (Regex.regex * (string -> token)) list
  end

(* Lexing module *)
module type S =
  sig
    type token
    val read_tokens : 
      Scanf.Scanning.in_channel ->
      int ->
      ((token * int) list, int * char) result
  end

module Make (T : TokenSig) : S with type token = T.token

module Regex = Regex
