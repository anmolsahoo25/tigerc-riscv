module T = Middle.Tree
module S = Syntax.SyntaxSig
module Temp = Middle.Temp

let translate = function
  | S.NilExp -> T.Const 0
  | S.IntExp i -> T.Const i
  | _ -> T.Const 0
