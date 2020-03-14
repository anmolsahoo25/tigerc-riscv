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

module Make (SS : SyntaxSig) = struct
  type parse_tree = SS.parse_tree
  type syntax_tree = SS.syntax_tree

  let get_sym_aux = let open SS in function
    | Leaf s -> s
    | Node (s, _) -> s

  let get_sym = let open SS in function
    | Leaf s -> [s]
    | Node (s,c) -> s :: (List.map get_sym_aux c)

  let rec gen_syntax_tree = let open SS in function
    | Leaf s -> SS.semantic_action [s] []
    | Node (_,c) as n ->
      let children = List.map gen_syntax_tree c in
      SS.semantic_action (get_sym n) children
end
