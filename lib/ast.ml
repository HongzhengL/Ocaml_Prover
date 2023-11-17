
type expr =
  | Id of string
  | Constructor of string * expr list option
  | TypeAnotation of string * string
  | FunctionCall of expr * expr
  (* function name, function return type (optinal) list of function parameters*)
  | FunctionSignature of string * string option * expr list
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

type hint = 
  | Axiom
  | Induction of string

type declaration =
    (*       name * variable list * expression * hint option        *)
  | Lemma of string * expr list option * expr * hint option
  | Type of string * expr list 
  | Function of expr * expr

