(* Functor for abstract syntax tree *)

module type S =
  sig
    type parse_tree
    type syntax_tree
    val  gen_syntax_tree : parse_tree -> syntax_tree
  end

module Make (SS : Parser.S) : S with type parse_tree = SS.parse_tree
