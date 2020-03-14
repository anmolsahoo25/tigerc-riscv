module ParserGen = Front.Parser.Make (Grammar)

module SyntaxSig = struct
  include ParserGen

  open Middle

  type symbol = Symbol.symbol

  type var =
    | SimpleVar of symbol
    | FieldVar of var * symbol
    | SubscriptVar of var * exp

  and exp =
    | NilExp
    | IntExp of int
    | StringExp of string
    | VarExp of var
    | CallExp of {func: symbol ; args: exp list}
    | OpExp of {left: exp ; oper: oper ; right: exp}
    | RecordExp of {fields: (symbol * exp) list ; typ: symbol}
    | SeqExp of exp list
    | AssignExp of {var: var ; exp: exp}
    | IfExp of {test: exp ; ifthen: exp ; ifelse: exp}
    | WhileExp of {test: exp ; body: exp}
    | ForExp of {var: symbol ; escape: bool ref; lo: exp; hi: exp; body: exp}
    | BreakExp
    | LetExp of {decs: dec list; body: exp}
    | ArrayExp of {typ: symbol; size: exp; init: exp}

  and dec =
    | FunDec of fundec list
    | VarDec of {name: symbol; escape: bool ref; typ: symbol option; init: exp}
    | TypeDec of (symbol * ty) list

  and ty =
    | NameTy of symbol
    | RecordTy of field list
    | ArrayTy of symbol

  and oper =
    | PlusOp | MinusOp | TimesOp | DivideOp | EqOp | NeqOp | LtOp
    | LeOp   | GtOp    | GeOp

  and field = {fname: symbol; escape: bool ref; typ: symbol}

  and fundec = {name: symbol; params: field list; res: symbol option; body: exp}

  type syntax_tree = exp

  let semantic_action _ _ = 
    NilExp
end

module SyntaxGen = Front.Syntax.Make (SyntaxSig)
