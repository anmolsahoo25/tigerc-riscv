module type S =
  sig
    type parse_tree
    type syntax_tree
    val  
  end

module Make (SS : Parser.S) = struct
  type parse_tree = SS.parse_tree
  type syntax_tree
end
