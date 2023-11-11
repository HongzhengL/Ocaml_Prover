type bop = Equal

type id = 
  | FuncID of string
  | TypeID of string
  | ParamID of string
  | LemmaID of string
  | VariantID of string

type expr=
  | Id of id
  | Variant of id * expr
  | TypeAnotation of id * id
  | TypeTuple of id list
  | FunctionHeader of expr * expr list
  | FunctionLeft of expr * expr
  | FunctionRight of expr * expr
  | Bop of bop * expr * expr

type declaration = 
  | Lemma of expr * expr
  | Type of id * expr list
  | Function of expr * expr

