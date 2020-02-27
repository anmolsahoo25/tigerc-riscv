type parse_tree = 
  | Empty
  | Leaf of Parser_gen.sym
  | Node of Parser_gen.sym * parse_tree list

let rec pprint_parse_tree (parent,id) = function
  | Empty -> ()
  | Leaf sym -> print_endline (
    "[" ^
    (string_of_int parent) ^
    "," ^
    (string_of_int id) ^
    "," ^
    (Parser_gen.pprint_sym sym) ^
    "]")
  | Node (sym, l) ->
      print_endline (
      "[" ^
      (string_of_int parent) ^
      "," ^
      (string_of_int id) ^
      "," ^
      (Parser_gen.pprint_sym sym) ^
      "]") ;
      List.iteri (fun i x -> pprint_parse_tree (id, id+i+1) x) l

let get_goto = function
  | Parser_gen.(Shift s) -> s
  | _ -> failwith "goto called on non-shift"

let rec popn stack n = match n with
  | 0 -> []
  | p ->
      let e = Stack.pop stack in
      (popn stack (p-1)) @ [e]

let parse_input parser_table input =
  let open Parser_gen in
  let stack = Stack.create () in
  Stack.push (Empty, 0) stack;
  let rec parse_input_aux state input =
    let scan_token = List.hd input in
    let action = Hashtbl.find_opt parser_table (state, scan_token) in
    match action with
    | Some Accept -> Some (fst (Stack.pop stack))
    | Some (Shift next) ->
        Printf.printf "%d, %s shift %d\n" state (Parser_gen.pprint_sym scan_token) next;
        Stack.push (Leaf scan_token, next) stack;
        parse_input_aux next (List.tl input)
    | Some (Reduce (sym, num)) ->
        let subtree = List.map fst (popn stack num) in
        let tree = Node (sym, subtree) in
        let (_, prev_state) = Stack.top stack in
        let next_state = get_goto (Hashtbl.find parser_table (prev_state, sym)) in
        Stack.push (tree, next_state) stack;
        Printf.printf "%d, %s reduce %d\n" state (Parser_gen.pprint_sym scan_token) next_state;
        parse_input_aux next_state input
    | None -> 
        Printf.printf "err: state : %d , token %s\n" state (Parser_gen.pprint_sym scan_token);
        None
  in
  parse_input_aux 0 input

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
