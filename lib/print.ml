let rec string_of_expr expr = match expr with
  | Ast.Identifyer s -> s
  | Ast.TypeAnotation (t, e) -> "(" ^ (string_of_expr e) ^ " : " ^ (match t with
    | Ast.Int -> "int") ^ ")"
  | Ast.FunctionHeader (e1, e2) ->
    (string_of_expr e1) ^ " " ^ (string_of_expr e2)
  | Ast.FunctionEval (e1, e2) ->
    (string_of_expr e1) ^ " (" ^ (string_of_expr e2) ^ ")"
  | Ast.Bop (bop, e1, e2) ->
    (string_of_expr e1) ^ 
    (match bop with Equal -> " = ")
    ^ (string_of_expr e2)
  

let string_of_declaration decl = match decl with 
  | Ast.Proof (e1, e2) -> "let (*prove*) " ^ (string_of_expr e1) ^ " = " ^ (string_of_expr e2)   

