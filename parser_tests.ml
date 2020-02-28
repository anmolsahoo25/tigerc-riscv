let grammar =
  let open Parser_gen in [
  { lhs = Nonterminal S ; rhs = [Nonterminal E ; Terminal Eof ] } ;
  { lhs = Nonterminal E ; rhs = [Nonterminal E ; Terminal Plus; Nonterminal T] } ;
  { lhs = Nonterminal E ; rhs = [Nonterminal T] } ;
  { lhs = Nonterminal T ; rhs = [Nonterminal T ; Terminal Star ; Nonterminal F] };
  { lhs = Nonterminal T ; rhs = [Nonterminal F] };
  { lhs = Nonterminal F ; rhs = [Terminal Lparen ; Nonterminal E ; Terminal Rparen] } ;
  { lhs = Nonterminal F ; rhs = [Terminal Id] }
]

let root_rule = List.hd grammar

let from_shift = fun x ->
  let open Parser_gen in
  match x with
    | Shift n -> n
    | Reduce _ -> -1
    | Accept -> 10000

let _ =
  let open Parser_gen in
  let inputs = 
    [Terminal Lparen ; Terminal Id ; Terminal Plus ; Terminal Id ; Terminal Rparen; Terminal Star ; Terminal Lparen ; Terminal Id ; Terminal Plus ; Terminal Id ; Terminal Rparen ; Terminal Eof] in
  let parser_table = Parser_gen.create_parser_table root_rule grammar in
  let ptree = parse_input parser_table inputs in
  match ptree with
  | Some s ->
    pprint_parse_tree (-1,0) s
  | None ->
    ()
  (*
  let table = parser_table in
  Hashtbl.iter (fun k v ->
    Printf.printf "%d , %s -> %d\n" (fst k) (Parser_gen.pprint_sym (snd k)) (from_shift v)) table;
  *)
