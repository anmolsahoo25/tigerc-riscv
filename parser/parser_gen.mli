type terminal_token = 
  | Plus
  | Star
  | Lparen
  | Rparen
  | Id
  | Eof

type nonterminal_token =
  | S
  | E
  | T
  | F

type sym = 
  | Terminal of terminal_token
  | Nonterminal of nonterminal_token

type rule = { lhs : sym ; rhs : sym list }

type action = 
  | Shift of int
  | Reduce of sym * int
  | Accept

type parser_item

val create_parser_table : rule -> rule list -> (int * sym , action) Hashtbl.t

val pprint_sym : sym -> string
