type expr =
  | Const of int
  | Temp of Temp.temp
  | Binop of binop * expr * expr
  | Mem of expr
  | Call of expr * expr list
  | Eseq of stm * expr

and stm =
  | Move of expr * expr
  | Exp of expr
  | Jump of expr * (Temp.label list)
  | Cjump of relop * expr * expr * Temp.label * Temp.label
  | Seq of stm * stm
  | Label of Temp.label

and binop =
  | Plus | Minus | Mul | Div | And | Or | Lshift | Rshift | Arshift
  | Xor

and relop =
  | Eq | Ne | Lt | Gt | Le | Ge | Ult | Ule | Ugt | Uge
