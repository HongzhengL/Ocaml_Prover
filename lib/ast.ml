
type expr =
  | Id of string
  | Constructor of string * expr option
  | TypeAnotation of string * string
  | ExprTuple of expr list
  | Function of expr * expr
  | Equal of expr * expr
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
    (*       name * variable list * expression *)
  | Lemma of string * expr list * expr
  | Type of string * expr list
  | RecFunction of expr * expr

