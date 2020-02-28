(* tokens to parse tree *)
module type GrammarSig =
  sig
    type token
    type nonterminal
    type sym
    type rule

    val pprint_token : token -> string
    val pprint_nonterminal : nonterminal -> string
    val pprint_sym : sym -> string
  end

module type S =
  sig
    type parse_tree
    type parser_table

    val parse_input : parser_table -> sym list -> parse_tree
  end


module Make
  (T : TokenSig)
  (S : GrammarSig with type token = T.token) : S
