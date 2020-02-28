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

let pprint_terminal_token = function
  | Plus -> "+"
  | Star -> "*"
  | Lparen -> "("
  | Rparen -> ")"
  | Id -> "id"
  | Eof -> "$"

let pprint_nonterminal_token = function
  | S -> "S"
  | E -> "E"
  | T -> "T"
  | F -> "F"

type sym = 
  | Terminal of terminal_token
  | Nonterminal of nonterminal_token

let pprint_sym = function
  | Terminal t -> pprint_terminal_token t
  | Nonterminal t -> pprint_nonterminal_token t


type rule = { lhs : sym ; rhs : sym list }

let find_all_tokens (grammar:rule list) =
  List.sort_uniq compare (
    List.flatten (List.map (fun x -> x.lhs :: x.rhs) grammar))

let first sym grammar =
  let rec first_aux sym = match sym with
  | Terminal t -> [Terminal t]
  | Nonterminal s ->
      let productions = List.filter (fun x -> x.lhs = Nonterminal s) grammar in
      let syms = List.map (fun x -> List.hd (x.rhs)) productions in
      List.flatten (List.map first_aux syms)
  in
  first_aux sym

let find_follow sym r grammar = 
  let rec find_follow_aux rhs =
  match rhs with
  | [] -> []
  | [p] -> []
  | h1 :: h2 :: tl ->
    if (h1 = h2) then
      (find_follow_aux (h2::tl))
    else if (h1 = sym) then
      (first h2 grammar) :: (find_follow_aux tl)
    else
      find_follow_aux (h2::tl)
  in
  List.flatten (find_follow_aux r.rhs)

let rec follow sym grammar =
  let r = List.filter (fun x -> List.mem sym (x.rhs)) grammar in
  let (re,rm) = 
    List.partition (fun x -> (List.nth x.rhs ((List.length x.rhs) - 1)) = sym) r in
  let re_f = List.flatten (List.map (fun x -> follow x.lhs grammar) re) in
  let re_m = List.flatten (
    (List.map (fun x -> find_follow sym x grammar) rm)) in
  List.sort_uniq compare (re_f @ re_m)

type parser_item = { root : sym ; seen : sym list ; ahead : sym list }

type grammar = rule list

type action = 
  | Shift of int
  | Reduce of sym * int
  | Accept

type parser_action = 
  | Shift of parser_item list
  | Reduce of rule
  | Accept
  

let pprint_parser_item {root ; seen ; ahead} = 
  let seen_string = List.fold_left (fun a b -> a ^ b) "" (List.map pprint_sym seen) in
  let ahead_string = List.fold_left (fun a b -> a ^ b) "" (List.map pprint_sym ahead) in
  (pprint_sym root) ^ "->" ^ seen_string ^ "O" ^ ahead_string

let pprint_parser_items items sym next =
  List.iter (fun x -> print_endline (pprint_parser_item x ^ "," ^ pprint_sym sym)) items;
  print_endline "||";
  List.iter (fun x -> print_endline (pprint_parser_item x ^ "," ^ pprint_sym sym)) next;
  print_endline "-------------"

let rec split_at_index n l =
  match n with 
  | 0 -> ([] , l)
  | p ->
    let hd = List.hd l in
    let (s1, s2) = split_at_index (p-1) (List.tl l) in
    (hd :: s1 , s2)

let create_parser_items rule =
  List.map (fun x ->
    let (s1,s2) = split_at_index x rule.rhs in
    { root = rule.lhs ; seen = s1 ; ahead = s2 })
  (List.init (List.length (rule.rhs) + 1) (fun x -> x))

let create_all_parser_items grammar =
  List.flatten (List.map (fun x -> create_parser_items x) grammar)

let closure_of_item item item_list =
  let add = try (match (List.hd item.ahead) with
  | Terminal _ -> []
  | Nonterminal _ -> 
      let next = List.hd item.ahead in
      List.filter (fun x -> x.root = next && x.seen = []) item_list)
  with Failure _ -> []
  in
  List.sort_uniq compare (item :: add)

let rec create_closure item_set item_list =
  let next_closure = List.sort_uniq compare (List.flatten (
    List.map (fun x -> closure_of_item x item_list) item_set)) in
  if (List.length next_closure) != (List.length item_set) then
    create_closure next_closure item_list 
  else
    next_closure

let next_state a item item_list =
  let has_next = List.filter (fun i -> i.ahead != []) item in
  let step_states = List.filter (fun i -> (List.hd i.ahead = a)) has_next
  in
  let next_states = List.map
    (fun x -> {
      root = x.root ;
      seen = x.seen @ [List.hd x.ahead] ;
      ahead = List.tl x.ahead}) step_states in
  create_closure next_states item_list

let simplify_table :
(parser_item list * sym , parser_action) Hashtbl.t -> 
(int * sym, action) Hashtbl.t = fun table ->
  let keys = ref [] in
  let indexes = Hashtbl.create 1024 in
  let ret_table : (int * sym , action) Hashtbl.t = Hashtbl.create 1024 in 
  Hashtbl.iter (fun k _ -> keys := (fst k) :: !keys) table;
  keys := List.sort_uniq compare !keys;
  List.iteri (fun i x -> Hashtbl.add indexes x i) !keys;
  Hashtbl.iter (fun k v ->
    let i1 = Hashtbl.find indexes (fst k) in
    let token = snd k in
    match v with
    | Shift s ->
      let i2 = Hashtbl.find indexes s in
      Hashtbl.add ret_table (i1, token) (Shift i2)
    | Reduce r ->
      let root = r.lhs in
      let num = List.length (r.rhs) in
      Hashtbl.add ret_table (i1, token) (Reduce (root, num))
    | Accept -> 
      Hashtbl.add ret_table (i1, token) Accept) table;
  ret_table

let create_parser_table (rule:rule) grammar =
  let table = Hashtbl.create 1024 in
  let init_item = {root = rule.lhs ; seen = [] ; ahead = rule.rhs} in
  let root_sym = init_item.root in
  let expr_sym = List.hd (init_item.ahead) in
  let item_list = create_all_parser_items grammar in
  let init_item = create_closure [init_item] item_list in
  let tokens = find_all_tokens grammar in
  let rec create_parser_table_aux item =
    (* check accept condition *)
    let term_condition = List.filter (fun x -> x.ahead = [Terminal Eof]) item in
    List.iter (fun x ->
      if (x.seen = [expr_sym] && x.root = root_sym) then begin
        if (not (Hashtbl.mem table (item, Terminal Eof))) then begin
          Hashtbl.add table (item, Terminal Eof) (Accept)
        end else begin
          ()
        end
      end else begin
      ()
      end) term_condition;
    (* add reduction rule first *)
    let red_rules = List.filter (fun x -> x.ahead = []) item in
    let follow_rules = List.map (fun x -> (x, follow (x.root) grammar)) red_rules in
    List.iter (fun (x, token) ->
      List.iter (fun y -> 
        if (not (Hashtbl.mem table (item,y))) then
          Hashtbl.add table (item, y) 
          (Reduce ({lhs = x.root ; rhs = x.seen @ x.ahead}))
        else
          ()) token
      ) follow_rules;
    let add_list = List.map (fun a -> 
      (next_state a item item_list, a)) tokens in
    let filtered_list = List.filter (fun x -> (fst x) != []) add_list in
    let not_already_bound = List.filter 
      (fun x -> not (Hashtbl.mem table (item, (snd x)))) filtered_list in
    if not_already_bound = [] then begin
      () 
    end else begin
      List.iter (fun x -> Hashtbl.add table (item, (snd x)) (Shift (fst x))) not_already_bound;
      List.iter (fun x -> create_parser_table_aux (fst x)) not_already_bound 
    end
  in
  create_parser_table_aux init_item;
  simplify_table table
