open Grammar
module M = Front.Parser.Make (Grammar)

include M

type expr =
  | Empty
  | AddOp of expr * expr
  | MulOp of expr * expr
  | Id

and

syntax_tree = expr

let semantic_action syms children = match syms with
  | [Nonterminal F ; Terminal (Id _)] -> Id
  | [Nonterminal T ; Nonterminal F] -> List.hd children
  | [Nonterminal E ; Nonterminal T] -> List.hd children
  | [Nonterminal F ; Terminal Lparen ; Nonterminal E ; Terminal Rparen] ->
      List.nth children 1
  | [Nonterminal E ; Nonterminal E ; Terminal (Op Plus) ; Nonterminal T] ->
      AddOp ((List.nth children 0), (List.nth children 2))
  | [Nonterminal T ; Nonterminal T ; Terminal (Op Star) ; Nonterminal F] ->
      MulOp ((List.nth children 0), (List.nth children 2))
  | _ -> Empty

let type_check _ = Ok ()
