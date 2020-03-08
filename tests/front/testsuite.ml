open OUnit2
open Grammar
open Syntaxsig
open Front

module Lexing = Lexer.Make (Token)
module Parsing = Parser.Make (Grammar)
module SyntaxGen = Syntax.Make (Syntaxsig)


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

let create_parser_test input_sym expected_tree =
  let f _ =
    let ptree = Parsing.parse_input input_sym in
    assert_equal ptree expected_tree
  in
  f

(* lexer tests *)
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

(* parser tests *)
let e_to_id_tree = 
  Node (Nonterminal E, [Node (Nonterminal T, [Node (Nonterminal F, [Leaf (Terminal (Id ""))])])])
let t_to_id_tree =
  Node (Nonterminal T, [Node (Nonterminal F, [Leaf (Terminal (Id ""))])])
let f_to_id_tree =
  Node (Nonterminal F, [Leaf (Terminal (Id ""))])
let f_to_paren_e_to_id_tree =
  Node (Nonterminal F, [Leaf (Terminal Lparen) ; e_to_id_tree ; Leaf (Terminal Rparen)])
let id_plus_id_tree =
  Node (Nonterminal E, [e_to_id_tree ; Leaf (Terminal (Op Plus)) ; t_to_id_tree])
let id_star_id_tree =
  Node (Nonterminal E, [Node (Nonterminal T, [t_to_id_tree ; Leaf (Terminal (Op Star)) ; f_to_id_tree])])
let id_star_id_plus_id_tree =
  Node (Nonterminal E, [
    Node (Nonterminal T, [
      t_to_id_tree ; 
      Leaf (Terminal (Op Star)) ;
      Node (Nonterminal F, [
        Leaf (Terminal Lparen) ;
        id_plus_id_tree ;
        Leaf (Terminal Rparen)])])])

let test_parser_single_id =
  create_parser_test
  [Terminal (Id "") ; Terminal Eof]
  (Some e_to_id_tree)

let test_parser_op_plus =
  create_parser_test
  [Terminal (Id "") ; Terminal (Op Plus) ; Terminal (Id "") ; Terminal Eof]
  (Some id_plus_id_tree)

let test_parser_op_star =
  create_parser_test
  [Terminal (Id "") ; Terminal (Op Star) ; Terminal (Id "") ; Terminal Eof]
  (Some id_star_id_tree)

let test_parser_id_star_id_plus_id =
  create_parser_test
  [Terminal (Id "") ; Terminal (Op Star) ; Terminal Lparen;
   Terminal (Id "") ; Terminal (Op Plus) ; Terminal (Id "") ; Terminal Rparen; Terminal Eof]
  (Some id_star_id_plus_id_tree)

(* ast tests *)
let test_ast_f_id _ =
  let parsed_tree = f_to_id_tree in
  let syntax_tree = SyntaxGen.gen_syntax_tree parsed_tree in
  let expect_tree = Syntaxsig.Id in
  assert_equal syntax_tree expect_tree

let test_ast_t_id _ =
  let parsed_tree = t_to_id_tree in
  let syntax_tree = SyntaxGen.gen_syntax_tree parsed_tree in
  let expect_tree = Syntaxsig.Id in
  assert_equal syntax_tree expect_tree

let test_ast_e_id _ =
  let parsed_tree = e_to_id_tree in
  let syntax_tree = SyntaxGen.gen_syntax_tree parsed_tree in
  let expect_tree = Syntaxsig.Id in
  assert_equal syntax_tree expect_tree

let test_ast_f_paren_e_id _ =
  let parsed_tree = f_to_paren_e_to_id_tree in
  let syntax_tree = SyntaxGen.gen_syntax_tree parsed_tree in
  let expect_tree = Syntaxsig.Id in
  assert_equal syntax_tree expect_tree

let test_ast_id_plus_id _ =
  let parsed_tree = id_plus_id_tree in
  let syntax_tree = SyntaxGen.gen_syntax_tree parsed_tree in
  let expect_tree = AddOp (Id, Id) in
  assert_equal syntax_tree expect_tree

let test_ast_id_star_id _ =
  let parsed_tree = id_star_id_tree in
  let syntax_tree = SyntaxGen.gen_syntax_tree parsed_tree in
  let expect_tree = MulOp (Id, Id) in
  assert_equal syntax_tree expect_tree

let test_ast_id_star_id_plus_id _ =
  let parsed_tree = id_star_id_plus_id_tree in
  let syntax_tree = SyntaxGen.gen_syntax_tree parsed_tree in
  let expect_tree = MulOp (Id, AddOp (Id, Id)) in
  assert_equal syntax_tree expect_tree

let suite = 
  "TestSuite" >::: [
    "test_lexer_if_else" >:: test_lexer_if_else ;
    "test_lexer_choice" >:: test_lexer_choice;
    "test_lexer_repeat" >:: test_lexer_repeat ;
    "test_parser_single_id" >:: test_parser_single_id ;
    "test_parser_op_plus" >:: test_parser_op_plus ;
    "test_parser_op_star" >:: test_parser_op_star ;
    "test_parser_id_star_id_plus_id" >:: test_parser_id_star_id_plus_id ;
    "test_ast_f_id" >:: test_ast_f_id ;
    "test_ast_t_id" >:: test_ast_t_id ;
    "test_ast_e_id" >:: test_ast_e_id ;
    "test_ast_f_paren_e_id" >:: test_ast_f_paren_e_id ;
    "test_ast_id_plus_id" >:: test_ast_id_plus_id ;
    "test_ast_id_star_id" >:: test_ast_id_star_id ;
    "test_ast_id_star_id_plus_id" >:: test_ast_id_star_id_plus_id
  ]

let () =
  run_test_tt_main suite
