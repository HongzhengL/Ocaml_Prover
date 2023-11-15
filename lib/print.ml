(*let string_of_id id = match id with 
  | Ast.FuncID s -> s
  | Ast.TypeID s -> s
  | Ast.ParamID s -> s
  | Ast.LemmaID s -> s
  | Ast.ConstructorID s -> s

let rec string_of_type_tuple id_list = match id_list with
  | [] -> ""
  | h::[] -> string_of_id h
  | h::tl -> string_of_id h ^ " * " ^ string_of_type_tuple tl


let rec string_of_expr expr = match expr with
  | Ast.Id s -> string_of_id s 
  | Ast.Constructor (id, e) -> string_of_id id ^ " " ^ string_of_expr e 
  | Ast.TypeConstructor (id, e) -> string_of_id id ^ " of " ^ string_of_expr e 
  | Ast.TypeAnotation (p, t) -> "(" ^ ( string_of_id p ) ^ " : " ^ ( string_of_id t ) ^ ")"
  | Ast.TypeTuple id_list -> "(" ^ string_of_type_tuple id_list ^ ")"
  | Ast.ExprTuple el -> "(" ^ string_of_expr_list_el el ^ ")"
  | Ast.FunctionHeader (name, ty, el) -> string_of_id name ^ " " ^ string_of_expr_list el ^ " : " ^ string_of_id ty
  | Ast.FunctionLeft (e1, e2) ->
    (string_of_expr e1) ^ " " ^ (string_of_expr e2)
  | Ast.FunctionRight (e1, e2) ->
    (string_of_expr e1) ^ ( match e2 with Ast.Id id -> " " ^ string_of_id id | _ -> " (" ^ (string_of_expr e2) ^ ")")
  | Ast.Bop (bop, e1, e2) ->
    "(" ^ (string_of_expr e1)  ^
    (match bop with Equal -> " = ") ^ 
     (string_of_expr e2) ^ ")" 
  | Match (e1, el) -> "match " ^ string_of_expr e1 ^ " with " ^ string_of_expr_list_m el 
  | Pattern (e1, e2) -> string_of_expr e1 ^ " -> " ^ string_of_expr e2
and string_of_expr_list_el lst = match lst with
  | [] -> ""
  | h::[] -> string_of_expr h
  | h::tl -> string_of_expr h ^ ", " ^string_of_expr_list_el tl
and string_of_expr_list_m lst = match lst with
  | [] -> ""
  | h::[] -> "| " ^ string_of_expr h
  | h::tl -> "| " ^ string_of_expr h ^ " " ^string_of_expr_list_m tl
and string_of_expr_list lst = match lst with
  | [] -> ""
  | h::[] -> string_of_expr h
  | h::tl -> string_of_expr h ^ " " ^ string_of_expr_list tl

let string_of_declaration decl = match decl with 
  | Ast.Lemma(e1, e2) -> "let (*prove*) "
   ^ (string_of_expr e1) ^ " = " ^ (string_of_expr e2)   
  | Ast.Type(id, el) -> "type " ^ string_of_id id ^ " = " ^ string_of_expr_list_m el 
  | Ast.Function(e1, e2) -> "let rec " ^ string_of_expr e1 ^ " = " ^ string_of_expr e2
  
*)
