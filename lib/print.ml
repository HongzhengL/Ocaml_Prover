let string_of_id id = match id with 
  | Ast.FuncID s -> s
  | Ast.TypeID s -> s
  | Ast.ParamID s -> s
  | Ast.LemmaID s -> s

let rec string_of_expr expr = match expr with
  | Ast.Id s -> string_of_id s
  | Ast.TypeAnotation (p, t) -> "(" ^ ( string_of_id p ) ^ " : " ^ ( string_of_id t ) ^ ")"
  | Ast.FunctionLeft (e1, e2) ->
    (string_of_expr e1) ^ " " ^ (string_of_expr e2)
  | Ast.FunctionRight (id, e2) ->
    (string_of_id id) ^ " (" ^ (string_of_expr e2) ^ ")"
  | Ast.Bop (bop, e1, e2) ->
    "(" ^ (string_of_expr e1)  ^
    (match bop with Equal -> " = ") ^ 
     (string_of_expr e2) ^ ")" 

let string_of_declaration decl = match decl with 
  | Ast.Proof (e1, e2) -> "let (*prove*) "
   ^ (string_of_expr e1) ^ " = " ^ (string_of_expr e2)   
  | _ -> ""

