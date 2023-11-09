type bop = Equal

type id = 
  | FuncID of string
  | TypeID of string
  | ParamID of string
  | LemmaID of string

type expr=
  | Id of id
  | TypeAnotation of id * id
  | FunctionLeft of expr * expr
  | FunctionRight of id * expr
  | Bop of bop * expr * expr

type declaration = 
  | Proof of expr * expr
  | Expr of expr

