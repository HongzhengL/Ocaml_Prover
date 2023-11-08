type type_name = Int 

type bop = Equal

type expr=
  | Identifyer of string
  | TypeAnotation of type_name * expr
  | FunctionHeader of expr * expr
  | FunctionEval of expr * expr
  | Bop of bop * expr * expr

type declaration = 
  | Proof of expr * expr

