open OUnit2
open Front
open Syntaxsig

module Lexing = Lexer.Make (Token)
module Parsing = Parser.Make (Grammar)
module SyntaxGen = Syntax.Make (Syntaxsig)

let int_prog _ =
  let prog = Prog [ConstInt 10] in
  let res = type_check prog in
  assert_equal res (Ok (TyInt))

let char_prog _ =
  let prog = Prog [ConstChar 'c'] in
  let res = type_check prog in
  assert_equal res (Ok (TyChar))

let assign_prog _ =
  let prog = Prog [VarDec (Id "x", ConstInt 10)] in
  let res = type_check prog in
  assert_equal res (Ok TyUnit)

let assign_prog_fail _ =
  let prog = Prog [VarDec (ConstInt 10, ConstInt 10)] in
  let res = type_check prog in
  assert_equal res (Error "")

let var_dec_and_ref _ =
  let prog = Prog [
    VarDec (Id "s", ConstInt 10) ;
    Var (Id "s")] in
  let res = type_check prog in
  assert_equal res (Ok TyInt)

let var_dec_and_ref_fail _ =
  let prog = Prog [
    VarDec (Id "t", ConstInt 10) ;
    Var (Id "s")] in
  let res = type_check prog in
  assert_equal res (Error "")

let var_dec_and_upd _ =
  let prog = Prog [
    VarDec (Id "t", ConstInt 10) ;
    VarUpd (Id "t", ConstInt 20)] in
  let res = type_check prog in
  assert_equal res (Ok TyUnit)

let var_dec_and_upd_tyfail _ =
  let prog = Prog [
    VarDec (Id "t", ConstInt 10) ;
    VarUpd (Id "t", ConstChar 'c')] in
  let res = type_check prog in
  assert_equal res (Error "")

let var_dec_and_upd_idfail _ =
  let prog = Prog [
    VarDec (Id "t", ConstInt 10) ;
    VarUpd (Id "s", ConstInt 20)] in
  let res = type_check prog in
  assert_equal res (Error "")

let addop_int _ =
  let prog = Prog [
    AddOp (ConstInt 10, ConstInt 10)] in
  let res = type_check prog in
  assert_equal res (Ok TyInt)

let addop_int_char _ =
  let prog = Prog [
    AddOp (ConstInt 10, ConstChar 'c')] in
  let res = type_check prog in
  assert_equal res (Error "")

let addop_var_var _ =
  let prog = Prog [
    VarDec (Id "t", ConstInt 10) ;
    VarDec (Id "s", ConstInt 20) ;
    AddOp (Id "t", Id "s")] in
  let res = type_check prog in
  assert_equal res (Ok TyInt)

let addop_var_char _ =
  let prog = Prog [
    VarDec (Id "t", ConstInt 10) ;
    AddOp (Id "t", ConstChar 'c')] in
  let res = type_check prog in
  assert_equal res (Error "")

let suite = 
  "TestSuite" >::: [
    "int_prog" >:: int_prog ;
    "char_prog" >:: char_prog ;
    "assign_prog" >:: assign_prog ;
    "assign_prog_fail" >:: assign_prog_fail ;
    "var_dec_and_ref" >:: var_dec_and_ref ;
    "var_dec_and_ref_fail" >:: var_dec_and_ref_fail ;
    "var_dec_and_upd" >:: var_dec_and_upd ;
    "var_dec_and_upd_tyfail" >:: var_dec_and_upd_tyfail ;
    "var_dec_and_upd_idfail" >:: var_dec_and_upd_idfail ;
    "addop_int" >:: addop_int ;
    "addop_int_char" >:: addop_int_char ;
    "addop_var_var" >:: addop_var_var ;
    "addop_var_char" >:: addop_var_char
  ]

let () =
  run_test_tt_main suite
