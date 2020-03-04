include Token

type nonterminal = 
  | S
  | E
  | T
  | F

type sym =
  | Terminal of token
  | Nonterminal of nonterminal

type rule = { lhs : sym ; rhs : sym list}

let pprint_terminal = function
  | _ -> ""

let pprint_nonterminal = function
  | _ -> ""

let grammar = [
  { lhs = Nonterminal S ; rhs = [Nonterminal E ; Terminal Eof ] } ;
  { lhs = Nonterminal E ; rhs = [Nonterminal E ; Terminal (Op Plus); Nonterminal T] } ;
  { lhs = Nonterminal E ; rhs = [Nonterminal T] } ;
  { lhs = Nonterminal T ; rhs = [Nonterminal T ; Terminal (Op Star); Nonterminal F] };
  { lhs = Nonterminal T ; rhs = [Nonterminal F] };
  { lhs = Nonterminal F ; rhs = [Terminal Lparen ; Nonterminal E ; Terminal Rparen] } ;
  { lhs = Nonterminal F ; rhs = [Terminal (Id "")] }
]

let eof = Terminal Eof
