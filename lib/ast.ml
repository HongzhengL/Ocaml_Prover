type bop = Equal

type id = 
  | FuncID of string
  | TypeID of string
  | ParamID of string
  | LemmaID of string
  | ConstructorID of string

type expr=
  | Id of id
  | Constructor of id * expr
  | TypeConstructor of id * expr
  | TypeAnotation of id * id
  | TypeTuple of id list
  | ExprTuple of expr list
  | FunctionHeader of id * id * expr list
  | FunctionLeft of expr * expr
  | FunctionRight of expr * expr
  | Bop of bop * expr * expr
  (* 
     thing we are matching * 
     pattern list 
  *)
  | Match of expr * expr list
  (*
     (constructor (expr1)) -> (expr2)
  *)
  | Pattern of expr * expr

type declaration = 
  | Lemma of expr * expr
  | Type of id * expr list
  | Function of expr * expr

