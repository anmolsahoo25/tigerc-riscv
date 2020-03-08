module Token = struct

  type token =
    | Eof

  let pprint_token = function
    | _ -> ""

  let token_regex = []
end

module Grammar = struct
  include Token

  type nonterminal =
    | S

  type sym =
    | Terminal of token
    | Nonterminal of nonterminal

  type rule = { lhs : sym ; rhs : sym list }

  let pprint_terminal = function
    | _ -> ""

  let pprint_nonterminal = function
    | _ -> ""

  let grammar = []

  let eof = Terminal Eof
end

module ParserGen = Frontend.Parser.Make (Grammar)

module SyntaxSig = struct
  include ParserGen

  type syntax_tree =
    | Empty

  let get_empty _ = Empty

  let semantic_action _ _ = Empty
end

module SyntaxGen = Frontend.Syntax.Make (SyntaxSig)
