open OUnit2
open Token

module Lexing = Lexer.Make (Token)

(* auxillary functions *)
let ic_from_string = Scanf.Scanning.from_string

let get_tokens ic  = 
  List.map fst (Result.get_ok (Lexing.read_tokens ic 0))

let create_lexer_test str expected_tokens =
  let f _ =
    let ic = ic_from_string str in
    let lexed_tokens = get_tokens ic in
    assert_equal lexed_tokens expected_tokens
  in f

(* tests *)
let test_lexer_if_else =
  create_lexer_test
  "if el if\n"
  [If ; El ; If]

let test_lexer_choice =
  create_lexer_test
  "+ - +\n"
  [Op Plus; Op Minus; Op Plus]

let test_lexer_repeat =
  create_lexer_test
    "i ii iii\n"
    [Id "i" ; Id "ii" ; Id "iii"]

let suite = 
  "TestSuite" >::: [
    "test_lexer_if_else" >:: test_lexer_if_else ;
    "test_lexer_choice" >:: test_lexer_choice;
    "test_lexer_repeat" >:: test_lexer_repeat
  ]

let () =
  run_test_tt_main suite
