module type GrammarSig =
  sig
    include Lexer.TokenSig

    type nonterminal
    type sym = Terminal of token | Nonterminal of nonterminal
    type rule = { lhs : sym ; rhs : sym list }

    val pprint_nonterminal : nonterminal -> string
    val grammar : rule list
    val eof : sym
  end

module type S =
  sig
    type sym

    type parse_tree = 
      | Empty
      | Leaf of sym
      | Node of sym * parse_tree list

    val parse_input : sym list -> parse_tree option
    val pprint_sym : sym -> string
  end

module Make (G : GrammarSig) = struct
  open G
  type sym = G.sym

  type parse_tree = 
    | Empty
    | Leaf of sym
    | Node of sym * parse_tree list

  type parser_item = { root : sym ; seen : sym list ; ahead : sym list }

  type action = 
    | Shift of int
    | Reduce of sym * int
    | Accept

  type parser_action = 
    | Shift of parser_item list
    | Reduce of rule
    | Accept

  let pprint_sym = function
    | Terminal t -> pprint_token t
    | Nonterminal t -> pprint_nonterminal t

  let rec split_at_index n l =
    match n with 
    | 0 -> ([] , l)
    | p ->
      let hd = List.hd l in
      let (s1, s2) = split_at_index (p-1) (List.tl l) in
      (hd :: s1 , s2)

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
    | [_] -> []
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

  let find_all_tokens (grammar:G.rule list) =
    List.sort_uniq compare (
      List.flatten (List.map (fun x -> x.lhs :: x.rhs) grammar))

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

  let get_goto : action -> int = function
    | Shift s -> s
    | _ -> failwith "goto called on non-shift"

  let rec popn stack n = match n with
    | 0 -> []
    | p ->
        let e = Stack.pop stack in
        (popn stack (p-1)) @ [e]

  let create_parser_table (rule:G.rule) grammar =
    let table = Hashtbl.create 1024 in
    let init_item = {root = rule.lhs ; seen = [] ; ahead = rule.rhs} in
    let root_sym = init_item.root in
    let expr_sym = List.hd (init_item.ahead) in
    let item_list = create_all_parser_items grammar in
    let init_item = create_closure [init_item] item_list in
    let tokens = find_all_tokens grammar in
    let rec create_parser_table_aux item =
      (* check accept condition *)
      let term_condition = List.filter (fun x -> x.ahead = [eof]) item in
      List.iter (fun x ->
        if (x.seen = [expr_sym] && x.root = root_sym) then begin
          if (not (Hashtbl.mem table (item, eof))) then begin
            Hashtbl.add table (item, eof) (Accept)
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

  let parse_input input =
    let stack = Stack.create () in
    let root_rule = List.hd (G.grammar) in
    let parser_table = create_parser_table root_rule G.grammar in
    Stack.push (Empty, 0) stack;
    let rec parse_input_aux state input =
      let scan_token = List.hd input in
      let action =
        Hashtbl.find_opt parser_table (state, scan_token) in
      match action with
      | Some Accept -> Some (fst (Stack.pop stack))
      | Some (Shift next) ->
          Stack.push (Leaf scan_token, next) stack;
          parse_input_aux next (List.tl input)
      | Some (Reduce (sym, num)) ->
          let subtree = List.map fst (popn stack num) in
          let tree = Node (sym, subtree) in
          let (_, prev_state) = Stack.top stack in
          let next_state =
            get_goto (Hashtbl.find parser_table (prev_state, sym)) in
          Stack.push (tree, next_state) stack;
          parse_input_aux next_state input
      | None -> 
          None
    in
    parse_input_aux 0 input
end

(*
type sym = 
  | Terminal of terminal_token
  | Nonterminal of nonterminal_token



type rule = { lhs : sym ; rhs : sym list }



type grammar = rule list

  

let pprint_parser_item {root ; seen ; ahead} = 
  let seen_string = List.fold_left (fun a b -> a ^ b) "" (List.map pprint_sym seen) in
  let ahead_string = List.fold_left (fun a b -> a ^ b) "" (List.map pprint_sym ahead) in
  (pprint_sym root) ^ "->" ^ seen_string ^ "O" ^ ahead_string

let pprint_parser_items items sym next =
  List.iter (fun x -> print_endline (pprint_parser_item x ^ "," ^ pprint_sym sym)) items;
  print_endline "||";
  List.iter (fun x -> print_endline (pprint_parser_item x ^ "," ^ pprint_sym sym)) next;
  print_endline "-------------"





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


*)
