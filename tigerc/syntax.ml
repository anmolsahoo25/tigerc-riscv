module ParserGen = Front.Parser.Make (Grammar)

module SyntaxSig = struct
  include ParserGen

  type syntax_tree =
    | Empty

  let get_empty _ = Empty

  let semantic_action _ _ = Empty

  let gen_treelang _ = Middle.Treelang.Empty
end

module SyntaxGen = Front.Syntax.Make (SyntaxSig)
