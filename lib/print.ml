let string_of_hint_option (h : Ast.hint option) = match h with 
  | Some Ast.Axiom ->  "(*hint: axiom *)"
  | Some Ast.Induction s -> "(*hint: induction " ^ s ^ " *)" 
  | None -> "" 


let rec string_of_expr expr = match expr with
  | Ast.Id s -> s 
  | Ast.TypeAnotation (name, t) -> "(" ^ name ^ " : " ^ t ^ ")"
  | Ast.Constructor (name, expr_list_opt) -> name ^ (match expr_list_opt with None -> "" | Some lst -> " (" ^ (string_of_expr_tuple lst) ^ ")" )
  | Ast.Function (e1, e2) -> string_of_expr e1 ^ 
    (match e2 with | Ast.Id s -> " " ^ s | Ast.Constructor (s, None) -> " " ^ s | _ -> " (" ^ string_of_expr e2 ^ ")")
  | Ast.Equal (e1, e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2
  | Ast.FunctionHeader (func_name, func_return_type, parameters) -> func_name ^ " " ^ (string_of_expr_list parameters) ^
    (match func_return_type with None -> "" | Some rt -> " : " ^ rt)
  | Match (thing_to_match, pattern_list) -> "match " ^ string_of_expr thing_to_match ^ " with\n" ^ string_of_variants pattern_list
  | Pattern (constructor, e) -> string_of_constructor_with_expr constructor ^ " -> " ^ string_of_expr e
  and string_of_expr_list expr_list = match expr_list with
  | [] -> ""
  | h::[] -> string_of_expr h
  | h::tl -> string_of_expr h ^ " " ^ string_of_expr_list tl
  and string_of_expr_tuple expr_list = match expr_list with
  | [] -> ""
  | h::[] -> string_of_expr h
  | h::tl -> string_of_expr h ^ ", " ^ string_of_expr_list tl
  and string_of_type_tuple id_list = match id_list with
| [] -> ""
| h::[] -> string_of_expr h
| h::tl -> string_of_expr h ^ " * " ^ string_of_type_tuple tl
  and string_of_variants ml = match ml with
    | [] -> ""
    | h::[] -> " | " ^ string_of_expr h
    | h::tl -> " | " ^ string_of_expr h ^ "\n" ^ string_of_variants tl
  and string_of_type_variants ml = match ml with
    | [] -> ""
    | h::tl -> " | " ^ string_of_constructor_with_types h ^ string_of_type_variants tl 
  and string_of_constructor_with_expr expr = match expr with 
  | Ast.Constructor (name, expr_list_opt) -> name ^ (match expr_list_opt with None -> "" | Some lst -> " (" ^ (string_of_expr_tuple lst) ^ ")")
  | _ -> ""
  and string_of_constructor_with_types expr = match expr with 
  | Ast.Constructor (name, expr_list_opt) -> name ^ (match expr_list_opt with None -> "" | Some lst -> " of (" ^ (string_of_type_tuple lst) ^ ")")
  | _ -> ""

let string_of_declaration decl = match decl with
  | Ast.Lemma (name, variables_opt, def, hint_opt) -> 
    "let (*prove*) " ^ name ^(match variables_opt with Some v -> " " ^(string_of_expr_list v) | _ -> "") 
    ^ " = " ^ (string_of_expr def) ^ " " ^ string_of_hint_option hint_opt
  | Ast.RecFunction (function_header, function_body) -> "let rec " ^ string_of_expr function_header ^ " = " ^ string_of_expr function_body
  | Ast.Type (type_name, type_tuple) -> "type " ^ type_name ^ " =" ^ (string_of_type_variants type_tuple)

(*let string_of_id id = match id with 
  | Ast.FuncID s -> s
  | Ast.TypeID s -> s
  | Ast.ParamID s -> s
  | Ast.LemmaID s -> s
  | Ast.ConstructorID s -> s


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
