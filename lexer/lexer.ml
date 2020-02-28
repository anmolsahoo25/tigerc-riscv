module type TokenSig = 
  sig
    type token
    val  pprint_token : token -> string
    val  token_regex  : (Regex.regex * (string -> token)) list
  end

module type S =
  sig
    type token
    val read_tokens : Scanf.Scanning.in_channel -> int -> ((token * int) list, int * char) result
  end

module Make (T : TokenSig) =
  struct
    type token = T.token

    exception Lexing_failed of int * char

    let sanitize_whitespace = function
      | '\t' -> '\n'
      | ' '  -> '\n'
      | c -> c

    let read_token ic start_pos =
      let match_string = ref "" in
      let regex_list =
        List.map (fun x -> (Regex.compile_re (fst x), (snd x))) T.token_regex in
      let rec read_token_aux regex_list pos =
        let c = Scanf.bscanf ic "%c" sanitize_whitespace in
        let step_regex = List.map (fun x -> (Regex.step_re (fst x) c, snd x)) regex_list in
        let filtered = List.filter (fun x -> (not (Option.is_none (fst x)))) step_regex in
        match (List.length filtered) with
        | 0 -> Error (pos, c)
        | _ ->
          let matched =
            List.filter (fun x ->
              (Regex.is_accept
                (snd (Option.get (fst x))))) filtered in
          begin match (List.length matched) with
          | 0 -> 
            let _ = (match_string := !match_string ^ (Char.escaped c)) in
            read_token_aux (List.map (fun x -> (Option.get (fst x), snd x)) filtered) (pos+1)
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
  end

module Regex = Regex
