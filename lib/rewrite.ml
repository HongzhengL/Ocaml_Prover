module Parser = Parser
module Lexer = Lexer
exception SyntaxError of string


let parse (s : string) : Ast.expr = 
    let lexbuf = Lexing.from_string s in
        let ast = Parser.expr_eof Lexer.read lexbuf in
            ast


module type Substitution = sig

end


module Substitution = struct
    module MM = Map.Make(String)
    type expression = Ast.expr

    type t = expression MM.t

    let empty = MM.empty

    let singleton key expr = MM.add key expr MM.empty

    let to_list (m : 'a MM.t) : (MM.key * 'a) list = MM.bindings m

    let rec merge_list l1 s2 = match l1 with
        | (key, value) :: t -> 
            (if (match (MM.find_opt key s2) with | Some e -> not (e = value)| None -> false) 
                then None 
            else merge_list t (MM.add key value s2))
        | [] -> Some s2

    let merge s1 s2 = merge_list (to_list s1) s2

    let find key s = MM.find_opt key s

    let rec get_variables exprs = match exprs with
        | h :: t -> 
            (match h with
            | Ast.Id s -> s :: get_variables t
            | Ast.TypeAnotation (e, _) -> e :: get_variables t
            | _ -> get_variables t)
        | [] -> []

    let rec match_expr variables pattern goal = match (pattern, goal) with
        | (Ast.Id p, Ast.Id g) -> 
            if List.mem p variables then Some (singleton p (Ast.Id g)) 
            else (if p = g then Some empty else None) 
        | (Ast.Id p, e) -> 
            if List.mem p variables then Some (singleton p e) else None
        | (Ast.FunctionCall (p_left, p_right), Ast.FunctionCall (g_left, g_right)) -> 
            (match (match_expr variables p_left g_left, match_expr variables p_right g_right) with
            | (Some s1, Some s2) -> merge s1 s2
            | (_, _) -> None)
        | (Ast.Constructor(nm_left, expr_list_opt_left), Ast.Constructor(nm_right, expr_list_opt_right)) -> 
            if nm_left = nm_right then
                (match (expr_list_opt_left, expr_list_opt_right) with
                | (Some expr_list_left, Some expr_list_right) -> 
                    match_expr_list variables expr_list_left expr_list_right
                | (None, None) -> Some empty
                | (_, _) -> None)
            else None
        | (_, _) -> None
    and match_expr_list variables expr_list_left expr_list_right = match (expr_list_left, expr_list_right) with
        | (h1 :: t1, h2 :: t2) -> 
            (match (match_expr variables h1 h2, match_expr_list variables t1 t2) with
            | (Some s1, Some s2) -> merge s1 s2
            | (_, _) -> None)
        | ([], []) -> Some empty
        | (_, _) -> None 

    let rec apply_single key value expr = match expr with
        | Ast.Id s -> if s = key then value else Ast.Id s
        | Ast.FunctionCall (e1, e2) -> Ast.FunctionCall (apply_single key value e1, apply_single key value e2)
        | Ast.Constructor (nm, expr_list_opt) -> 
            (match expr_list_opt with
            | Some expr_list -> Ast.Constructor (nm, Some (apply_single_list key value expr_list))
            | None -> Ast.Constructor (nm, None))
        | expr -> expr
    and apply_single_list key value expr_list = match expr_list with
        | h :: t -> (apply_single key value h) :: apply_single_list key value t
        | [] -> []

    let apply sub expr = 
        let rec apply_helper sub expr = match sub with
            | (key, value) :: t -> apply_helper t (apply_single key value expr)
            | [] -> expr
        in
        apply_helper (to_list sub) expr
end


let rec attemptRewrite (variables : Ast.expr list) (equality : Ast.expr) (expr : Ast.expr) : Ast.expr option =
    let rewriteFunctionCall left right =
        match (attemptRewrite variables equality left, attemptRewrite variables equality right) with
        | (Some rewrittenLeft, None) -> Some (Ast.FunctionCall (rewrittenLeft, right))
        | (None, Some rewrittenRight) -> Some (Ast.FunctionCall (left, rewrittenRight))
        | _ -> None
    in
    match equality with
    | Ast.Equal(e1, e2) -> 
        begin
            match Substitution.match_expr (Substitution.get_variables variables) e1 expr with
            | Some sub -> Some (Substitution.apply sub e2)
            | None -> 
                begin
                    match expr with
                    | Ast.FunctionCall (left, right) -> rewriteFunctionCall left right
                    | _ -> None 
                end
        end
    | _ -> None


let getVar (vars : Ast.expr list option) = match vars with
    | Some e -> e
    | None -> []


let rec tryEqualities (expr : Ast.expr) (equalities : (string * Ast.expr list option * Ast.expr) list) : (string * Ast.expr) option =
    let attemptRewriteWithEquality (name, variables, equality) =
        match attemptRewrite (getVar variables) equality expr with
        | Some e -> Some (name, e)
        | None -> None
    in
    match equalities with
    | [] -> None
    | head :: tail -> 
        match attemptRewriteWithEquality head with
        | Some result -> Some result
        | None -> tryEqualities expr tail


let rec performSteps (expr : Ast.expr) (equalities : (string * Ast.expr list option * Ast.expr) list) : (string * Ast.expr) list =
    let applyEqualityAndRecurse (name, rewrittenExpr) =
        (name, rewrittenExpr) :: performSteps rewrittenExpr equalities
    in
    match tryEqualities expr equalities with
    | Some equalityResult -> applyEqualityAndRecurse equalityResult
    | None -> []
       
    
let containsDefinition (s : string) = 
    try
      Str.search_forward (Str.regexp_string "definition") s 0 |> ignore;
      true
    with Not_found -> false


let rec printSteps (steps : (string * Ast.expr) list) : string list =
    match steps with
    | (name, expr) :: t -> 
        ("= { " ^ name ^ " }\n" ^ Print.string_of_expr expr ^ "\n") :: printSteps t
    | [] -> []
    

let rec printRightSteps (steps : (string * Ast.expr) list) : string list =
    match steps with
    | (name1, _) :: (name2, expr2) :: tail -> 
        ("= { " ^ name1 ^ " }\n" ^ Print.string_of_expr expr2 ^ "\n") :: printRightSteps ((name2, expr2) :: tail)
    | [(name, _)] -> ["= { " ^ name ^ " }\n"]
    | [] -> []


let produceProof (equality : Ast.expr) (equalities : (string * Ast.expr list option * Ast.expr) list) : string list =
    match equality with
    | Equal (left, right) -> 
        let left_steps = performSteps left equalities in
        let right_steps = performSteps right equalities in
        let steps_non_empty = List.length left_steps > 0 && List.length right_steps > 0 in
        let only_left_steps = List.length left_steps > 0 && List.length right_steps = 0 in 
        if steps_non_empty then
            (printSteps left_steps) @ (printRightSteps right_steps) @ [Print.string_of_expr right ^ "\n"]
        else if only_left_steps then
            printSteps left_steps
        else
            ["= { ??? }\n" ^ Print.string_of_expr right ^ "\n"]
    | _ -> raise (SyntaxError "produceProof: Expected an equality")


let processLemma (name, (def : Ast.expr), (hint_opt : Ast.hint option)) (lemma_list : (string * Ast.expr list option * Ast.expr) list) =
    let processEquality e1 e2 hintDescription = 
        name ^ " is assumed as an axiom:\n" ^ Print.string_of_expr e1 
        ^ "\n= { " ^ hintDescription ^ " }\n" ^ Print.string_of_expr e2 ^ "\n"
    in
    match def with
    | Ast.Equal (e1, e2) -> (
        match hint_opt with
        | Some Axiom -> processEquality e1 e2 "axiom"
        | Some (Induction x) -> processEquality e1 e2 (" is proved by induction on " ^ x ^ ":\n")
        | None -> "Proof of " ^ name ^ ":\n" ^ Print.string_of_expr e1 ^ "\n" ^ String.concat "" (produceProof def lemma_list)
    )
    | _ -> raise (SyntaxError "processLemma: Expected an equality")


let rec print_lemma lemma_list =
    match lemma_list with
    | [] -> ""
    | (name, _, def) :: tail -> 
        name ^ " : " ^ Print.string_of_expr def ^ "\n" ^ print_lemma tail


let rec tuple_to_string (t : Ast.expr list) : string =
    match t with
    | [] -> ""
    | [Ast.TypeAnotation(name, _)] -> name
    | Ast.TypeAnotation(name, _) :: tail -> name ^ ", " ^ tuple_to_string tail
    | _ -> raise (SyntaxError "tuple_to_string: Expected a type anotation")
        

let getCons(expr : Ast.expr) : string =
    match expr with
    | Ast.Constructor (nm, None) -> nm
    | Ast.Constructor (nm, Some tuple) -> 
        nm ^ " (" ^ tuple_to_string tuple ^ ")"
    | _ -> raise (SyntaxError "getCons: Expected a constructor")
    

let rec getParams (params : Ast.expr) : string =
    match params with
    | Ast.FunctionSignature(name, type_name, param) -> 
        begin
            match param with
            | [] -> ""
            | Ast.Id s :: t | Ast.TypeAnotation (s, _) :: t -> 
                s ^ " " ^ getParams (Ast.FunctionSignature(name, type_name, t))
            | _ -> raise (SyntaxError "getParams: Expected an id or type anotation")
        end
    | _ -> raise (SyntaxError "getParams: Expected a function signature")
    

let rec getFunctionRec (name : string) (function_header : Ast.expr) (function_body : Ast.expr) : Ast.expr list =
    let processPattern (pat, e) expr =
        let substituted = Substitution.apply (Substitution.singleton (Print.string_of_expr expr) (parse (getCons pat))) (parse (name ^ " " ^ getParams function_header)) in
        parse (Print.string_of_expr substituted ^ " = " ^ Print.string_of_expr e)
    in

    match function_body with
    | Ast.Match (expr, expr_list) -> (
        match expr_list with
        | Pattern(left, e) :: tail -> 
            processPattern (left, e) expr :: getFunctionRec name function_header (Ast.Match(expr, tail))
        | [] -> []
        | _ -> raise (SyntaxError "getFunctionRec: Expected a pattern")
    )
    | _ -> raise (SyntaxError "getFunctionRec: Expected a match expression")
  
    
let getFunction (name : string) (function_header : Ast.expr) (function_body : Ast.expr) =
    let formatMatchCase (left, e) = 
        " |" ^ Print.string_of_expr left ^ " -> " ^ Print.string_of_expr e 
    in

    let rec formatMatchCases expr_list = 
        match expr_list with
        | Ast.Pattern(left, e) :: tail -> formatMatchCase (left, e) ^ formatMatchCases tail
        | [] -> ""
        | _ -> raise (SyntaxError "getFunction: Expected a pattern")
    in

    match function_body with
    | Ast.Match (expr, expr_list) ->
        let left_eq = name ^ " " ^ getParams function_header in
        let right_string = "match " ^ Print.string_of_expr expr ^ " with" in 
        parse (left_eq ^ " = " ^ right_string ^ formatMatchCases expr_list)
    | _ -> raise (SyntaxError "getFunction: Expected a match expression")
   
    
let produce_output_simple (decls : Ast.declaration list) : string =
    (* Nested helper function to process each declaration *)
    let rec processDecls decls acc =
        match decls with
        | [] -> "" (* Placeholder for final accumulator processing *)
        | h :: t ->
            match h with
            | Ast.Lemma(name, variables, def, hint_opt) ->
                let lemma_entry = ("lemma " ^ name, variables, def) in
                let result = processLemma (name, def, hint_opt) acc ^ "\n" in
                result ^ processDecls t (acc @ [lemma_entry])
  
            | Ast.Type (_, _) ->
                failwith "unimplemented"
  
            | Ast.Function (function_header, function_body) ->
                processFunction function_header function_body acc t
  
    (* Helper function to process function declarations *)
    and processFunction function_header function_body acc t =
        match function_header with
        | Ast.FunctionSignature (name, _, variables) ->
            let function_records = getFunctionRec name function_header function_body in
            let new_acc_entries = List.map (fun record -> ("definition of " ^ name, Some variables, record)) function_records in
            (* let function_records = getFunction name function_header function_body in *)
            (* let new_acc_entries = [("definition of " ^ name, Some variables, function_records)] in *)
            processDecls t (acc @ new_acc_entries)

        | _ -> raise (SyntaxError "produce_output_simple: Expected a function signature")
    in
    processDecls decls []
  
  