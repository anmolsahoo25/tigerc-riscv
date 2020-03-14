module ParserGen = Front.Parser.Make (Grammar)

module SyntaxSig = struct
  include ParserGen

  type syntax_tree =
    | Empty

  let semantic_action _ _ = Empty
end

module SyntaxGen = Front.Syntax.Make (SyntaxSig)
