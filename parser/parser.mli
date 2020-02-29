(* tokens to parse tree *)
module type GrammarSig =
  sig
    include Lexer.TokenSig

    type nonterminal
    type sym = Terminal of token | Nonterminal of nonterminal
    type rule = { lhs : sym ; rhs : sym list }

    val pprint_nonterminal : nonterminal -> string
    val grammar : rule list
    val eof : sym
  end

module type S =
  sig
    type sym
    type parse_tree = 
      | Empty
      | Leaf of sym
      | Node of sym * parse_tree list

    val parse_input : sym list -> parse_tree option
    val gen_ast     : parse_tree -> Syntax.syntax_tree
    val pprint_sym : sym -> string
  end

module Make (G : GrammarSig) : S with type sym = G.sym
