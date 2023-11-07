type type_name = Int | String

type expr =
  | Var of string
  | TypeDecl of type_name * string
