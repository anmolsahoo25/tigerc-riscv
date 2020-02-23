module CharSet = Set.Make(Char)
module IntSet = Set.Make(Int)

type regex =
  | Empty
  | Symbol of char
  | Concat of regex * regex
  | Choice of regex * regex
  | Repeat of regex

let rec print_regex = function
  | Empty           -> "_"
  | Symbol c        -> Printf.sprintf "%c" c
  | Concat (r1, r2) -> "(" ^ (print_regex r1) ^ (print_regex r2) ^ ")"
  | Choice (r1, r2) -> (print_regex r1) ^ " | " ^ (print_regex r2)
  | Repeat r1       -> Printf.sprintf "%s*" (print_regex r1)

let rec get_alpha = function
  | Empty -> CharSet.empty
  | Symbol c -> CharSet.singleton c
  | Concat (r1, r2)
  | Choice (r1, r2) -> CharSet.union (get_alpha r1) (get_alpha r2)
  | Repeat r1 -> (get_alpha r1)

let to_nfa r =
  let rec to_nfa_aux init = function
    | Empty           -> ([(init, None, init+1)], init+1)
    | Symbol c        -> ([(init, Some c, init+1)], init+1)
    | Concat (r1, r2) ->
      let nfa1, e1 = to_nfa_aux init r1 in
      let nfa2, e2 = to_nfa_aux e1 r2 in
      (nfa1 @ nfa2, e2)
    | Choice (r1, r2) ->
      let ci1 = [(init, None, init+1)] in
      let nfa1, e1 = to_nfa_aux (init+1) r1 in
      let ci2 = [(init, None, e1+1)] in
      let nfa2, e2 = to_nfa_aux (e1+1) r2 in
      let ce1 = [(e1, None, e2+1)] in
      let ce2 = [(e2, None, e2+1)] in
      (ci1 @ ci2 @ nfa1 @ nfa2 @ ce1 @ ce2 , e2+1)
    | Repeat r1 ->
      let ci1 = [(init, None, init+1)] in
      let nfa1, e1 = to_nfa_aux (init+1) r1 in
      let loop = [(e1, None, init+1)] in
      let ce1 = [(e1, None, e1+1)] in
      let exit = [(init, None, e1+1)] in
      (ci1 @ nfa1 @ loop @ ce1 @ exit, e1 + 1)
  in
  let trans, end_state = to_nfa_aux 0 r in
  let nfa = Hashtbl.create 1024 in
  List.iter (fun (i,c,n) -> Hashtbl.add nfa (i,c) n) trans;
  (nfa, end_state)

let rec reachable_states_from_one nfa state sym =
  let sym_reachable = Hashtbl.find_all nfa (state, sym) in
  let eps_reachable =
    List.map (fun x -> reachable_states_from_one nfa x None)
    sym_reachable in
  sym_reachable @ (List.flatten eps_reachable)

let reachable_states nfa states sym =
  List.flatten
  (List.map (fun x -> reachable_states_from_one nfa x sym) states)

let to_dfa r = 
  let dfa = Hashtbl.create 1024 in
  let alpha = CharSet.elements (get_alpha r) in
  let nfa, end_state = to_nfa r in

  let rec to_dfa_aux init =
    let init_states = IntSet.elements init in
    let next_states =
      List.map (fun x ->
      (x, IntSet.of_list (reachable_states nfa init_states (Some x))))
      alpha in
    let next_states = 
      List.filter (fun x -> not (IntSet.is_empty (snd x)))
      next_states in
    List.iter (fun (c, s) ->
      Hashtbl.add dfa (init, c) s;
      if (IntSet.mem end_state s) then
        Hashtbl.add dfa (s, '\n') (IntSet.singleton (-1))
      else ();
      to_dfa_aux s) next_states
  in
  let init_states = IntSet.of_list ([0] @ (reachable_states nfa [0] None)) in
  to_dfa_aux init_states;
  (dfa, init_states)

let compile_re re = to_dfa re

let step_re (dfa, state) sym = 
  match (Hashtbl.find_opt dfa (state, sym)) with
  | Some n -> Some (dfa, n)
  | None   -> None
