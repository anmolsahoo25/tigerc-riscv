type token =
  | Eof

let pprint_token = function
  | _ -> ""

let token_regex = []

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
