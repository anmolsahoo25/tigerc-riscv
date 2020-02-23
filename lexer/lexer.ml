open Regex
open Result
open Option

exception Lexing_failed of int * char

type token = 
  | IF
  | IFF
  | LET

let print_token = function
  | IF -> print_endline "if"
  | IFF -> print_endline "iff"
  | LET -> print_endline "let"

let token_regex = [
  (Concat (Symbol 'i', Symbol 'f'), fun _ -> IF) ;
  (Concat (Concat (Symbol 'i', Symbol 'f'), Symbol 'f'), fun _ -> IFF) ;
  (Concat (Concat (Symbol 'l', Symbol 'e'), Symbol 't'), fun _ -> LET)
]

let sanitize_whitespace = function
  | '\t' -> '\n'
  | ' '  -> '\n'
  | c -> c

let read_token ic start_pos =
  let match_string = ref "" in
  let regex_list =
    List.map (fun x -> (compile_re (fst x), (snd x))) token_regex in
  let rec read_token_aux regex_list pos =
    let c = Scanf.bscanf Scanf.Scanning.stdin "%c" sanitize_whitespace in
    let step_regex = List.map (fun x -> (step_re (fst x) c, snd x)) regex_list in
    let filtered = List.filter (fun x -> (not (is_none (fst x)))) step_regex in
    match (List.length filtered) with
    | 0 -> Error (pos, c)
    | _ ->
      let matched =
        List.filter (fun x -> (IntSet.singleton (-1) = (snd (get (fst x))))) filtered in
      begin match (List.length matched) with
      | 0 -> 
        let _ = (match_string := !match_string ^ (Char.escaped c)) in
        read_token_aux (List.map (fun x -> (get (fst x), snd x)) filtered) (pos+1)
      | _ -> 
        Ok ((snd (List.hd (matched))) !match_string, pos)
      end
  in
  read_token_aux regex_list start_pos

let rec read_tokens_aux ic pos =
  if (Scanf.Scanning.end_of_input ic) then
    []
  else
    match (read_token ic pos) with
    | Ok (tok1, pos1) -> (tok1, pos1) :: (read_tokens_aux ic (pos1+1))
    | Error (pos, c) -> raise (Lexing_failed (pos,c))

let read_tokens ic pos =
  try
    Ok (read_tokens_aux ic pos) 
  with
    Lexing_failed (pos, c) -> Error (pos, c)
