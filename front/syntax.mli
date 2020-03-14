module type SyntaxSig =
  sig
    include Parser.S
    type syntax_tree
    val  semantic_action : sym list -> syntax_tree list -> syntax_tree
  end

module type S = sig
  type parse_tree
  type syntax_tree

  val gen_syntax_tree : parse_tree -> syntax_tree
end

module Make (SS : SyntaxSig) : S
with type parse_tree = SS.parse_tree
with type syntax_tree = SS.syntax_tree
