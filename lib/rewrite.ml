module Parser = Parser
module Lexer = Lexer

let parse (s : string) : Ast.expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.expr_eof Lexer.read lexbuf in
     ast

module type Substitution = sig
  type expression  (* You need to define this type *)
  
  type t = expression Map.Make(String).t
  val empty : t
  val singleton : string -> expression -> t
  val merge : t -> t -> t option
  val find : string -> t -> expression option
  val match_expr : expression list -> expression -> expression -> t option

end

module Substitution = struct
  module MM = Map.Make(String)
  type expression = Ast.expr
  
  type t = expression MM.t

  let empty = MM.empty

  let singleton key expr = MM.add key expr MM.empty

  let to_list (m : 'a MM.t) : (MM.key * 'a) list = MM.bindings m

  let rec merge_list l1 s2 = match l1 with
    | (k, v) :: t -> (if (match (MM.find_opt k s2) with
      | Some e -> not (e = v)
      | None -> false) then None else merge_list t (MM.add k v s2))
    | [] -> Some s2

  let merge s1 s2 = merge_list (to_list s1) s2

  let find key s = MM.find_opt key s

  let rec get_variables exprs = match exprs with
    | h :: t -> (match h with
      | Ast.Id s -> s :: get_variables t
      | Ast.TypeAnotation (e, _) -> e :: get_variables t
      | _ -> get_variables t)
    | [] -> []

  let rec match_expr variables pattern goal = match (pattern, goal) with
    | (Ast.Id p, Ast.Id g) -> if List.mem p variables then Some (singleton p (Ast.Id g)) 
                              else (if p = g then Some empty else None) 
    | (Ast.Id p, e) -> if List.mem p variables then Some (singleton p e) else None
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
      | (None, None) -> merge empty empty
      | (_, _) -> None)
    | ([], []) -> Some empty
    | (_, _) -> None 

  let rec apply_single key value expr = match expr with
  | Ast.Id s -> if s = key then value else Ast.Id s
  | Ast.FunctionCall (e1, e2) -> Ast.FunctionCall (apply_single key value e1, apply_single key value e2)
  | Constructor (nm, expr_list_opt) -> 
    (match expr_list_opt with
    | Some expr_list -> Constructor (nm, Some (apply_single_list key value expr_list))
    | None -> Constructor (nm, None))
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

(* 
* attemptRewrite : string list -> equality -> expression -> expression option
    Given a list of variables, an equality to apply, and an expression to apply it to,
    returns the result to applying the equality to the expression (if possible)
* tryEqualities : expression -> (string * string list * equality) list -> (string * expression) option
    Given an expression, tries to apply each equality in the list. Returns the name of
    the rule that was successfully applied and the resulting expression
* performSteps : expression -> (string * string list * equality) list -> (string * expression) list
    Applies tryEqualities on the new expression until it returns None, gathers the
    results as a list
* produceProof : equality -> (string * string list * equality) list -> string list
    Attempts to prove the equality by breaking up the lhs and the rhs and calling
    performSteps on each side. Inserts ??? if the lhs and rhs end up staying different
* produce_output_simple : declaration list -> string 
*)

let rec attemptRewrite (variables : Ast.expr list) (equality : Ast.expr) (expr : Ast.expr) : Ast.expr option =
  match equality with
  | Ast.Equal(e1, e2) -> 
    (match Substitution.match_expr (Substitution.get_variables variables) e1 expr with
    | Some sub -> Some (Substitution.apply sub e2)
    | None -> (
      match expr with
      | Ast.FunctionCall (left, right) -> (
        match (attemptRewrite variables equality left, attemptRewrite variables equality right) with
        | (Some rst, None) -> Some (Ast.FunctionCall (rst, right))
        | (None, Some rst) -> Some (Ast.FunctionCall (left, rst))
        | (None, None) -> None
        | (Some _, Some _) -> None
      )
      | _ -> None 
    )
    )
  | _ -> None


let getVar (vars : Ast.expr list option) = match vars with
  | Some e -> e
  | None -> []

(* 
 * tryEqualities : expression -> (string * string list * equality) list -> (string * expression) option
 *   Given an expression, tries to apply each equality in the list. Returns the name of
 *   the rule that was successfully applied and the resulting expression   
*)
let rec tryEqualities (expr : Ast.expr) (equalities : (string * Ast.expr list option * Ast.expr) list) : (string * Ast.expr) option =
  match equalities with
    | (name, variables, equality) :: t -> 
        (match attemptRewrite (getVar variables) equality expr with
        | Some e -> Some (name, e)
        | None -> tryEqualities expr t)
    | [] -> None

let rec performSteps (expr : Ast.expr) (equalities : (string * Ast.expr list option * Ast.expr) list) : (string * Ast.expr) list =
  match tryEqualities expr equalities with
  | Some (name, e) -> (name, e) :: performSteps e equalities
  | None -> []
  
let rec printSteps (steps : (string * Ast.expr) list) : string list =
  match steps with
  | (name, expr) :: t -> ("= { " ^ name ^ " }\n" ^ Print.string_of_expr expr ^ "\n") :: printSteps t
  | [] -> []

let rec printRightSteps (steps : (string * Ast.expr) list) : string list =
  match steps with
  | (name1, _) :: (name2, expr2) :: t -> ("= { " ^ name1 ^ " }\n" ^ Print.string_of_expr expr2 ^ "\n") :: printRightSteps ((name2, expr2)::t)
  | [(name, _)] -> ("= { " ^ name ^ " }\n") :: []
  | [] -> []

let rec verifyRHS (expr : Ast.expr) (steps : (string * Ast.expr) list) : bool =
  match steps with
  | [(_, e)] -> expr = e
  | _ :: t -> verifyRHS expr t
  | [] -> false

exception SyntaxError of string
let produceProof (equality : Ast.expr) (equalities : (string * Ast.expr list option * Ast.expr) list) : string list =
  match equality with
  | Equal (left, right) -> 
    (let left_steps =  performSteps left equalities in
    let right_steps = performSteps right equalities in
      if List.length left_steps > 0  then
        (printSteps left_steps) @ (printSteps right_steps)
      else
        ["= { ??? }\n" ^ Print.string_of_expr right ^ "\n"]
    )
  | _ -> raise (SyntaxError "Expected an equality")
  
let rec list_to_string (l : string list) : string =
  match l with
  | h :: t -> h ^ list_to_string t
  | [] -> ""

(* let processLemma (name, (variables : Ast.expr list option), (def : Ast.expr), (hint_opt : Ast.hint option)) (lemma_list : (string * Ast.expr list option * Ast.expr) list)= *)
let processLemma (name, (def : Ast.expr), (hint_opt : Ast.hint option)) (lemma_list : (string * Ast.expr list option * Ast.expr) list)=
  match hint_opt with
  | Some Axiom -> 
      (match def with
      | Ast.Equal (e1, e2) -> 
        name ^ " is assumed as an axiom:\n" ^ Print.string_of_expr e1 
        ^ "\n= {axiom}\n" ^ Print.string_of_expr e2 ^ "\n"
      | _ -> failwith "Expected an equality, but " ^ Print.string_of_expr def ^ " was given")
  | Some (Induction x) -> 
      (match def with
      | Ast.Equal (e1, e2) -> 
        name ^ " is proved by induction on " ^ x ^ ":\n" ^ Print.string_of_expr e1 
        ^ "\n= {induction on " ^ x ^ "}\n" ^ Print.string_of_expr e2 ^ "\n"
      | _ -> failwith "Expected an equality, but " ^ Print.string_of_expr def ^ " was given")
  | None -> 
      (match def with
      | Ast.Equal (e1, _) -> "Proof of " ^ name ^ ":\n" ^
        Print.string_of_expr e1 ^ "\n" ^ list_to_string (produceProof def lemma_list)
      | _ -> failwith "Expected an equality, but " ^ Print.string_of_expr def ^ " was given"
      )

(* Debug function *)
let rec print_lemma lemma_list =
  match lemma_list with
  | (name, _, def) :: t -> name ^ " : " ^ Print.string_of_expr def ^ "\n" ^ print_lemma t
  | [] -> ""

(* let getFunctionSimple (function_header : Ast.function_header) (function_body : Ast.expr) : (string * Ast.expr list option * Ast.expr) list =
  match function_header with
  | Ast.FunctionSignature (name, _, variables) -> [(name, Some variables, function_body)]
  | _ -> raise (SyntaxError "Expected a function signature") *)
  
let rec tuple_to_string (t : Ast.expr list) : string =
  match t with
  | [Ast.TypeAnotation(name, _)] -> name
  | Ast.TypeAnotation(name, _) :: t -> name ^ ", " ^ tuple_to_string t
  | [] -> ""
  | _ -> raise (SyntaxError "tuple_to_string: Expected a type anotation")

let getCons(expr : Ast.expr) : string =
  match expr with
  | Ast.Constructor (nm, None) -> nm
  | Ast.Constructor (nm, Some tuple) -> 
    nm ^ " (" ^ (tuple_to_string tuple) ^ ")"
  | _ -> raise (SyntaxError "getCons: Expected a constructor")

let rec getParams (params : Ast.expr) : string =
  match params with
  | Ast.FunctionSignature(name, type_name, param) -> 
    (match param with
    | Ast.Id s :: t -> s ^ " " ^ getParams (Ast.FunctionSignature(name, type_name, t))
    | Ast.TypeAnotation (s, _) :: t -> s ^ " " ^ getParams (Ast.FunctionSignature(name, type_name, t))
    | [] -> ""
    | _ -> raise (SyntaxError "getParams: Expected an id")
    )
  | _ -> raise (SyntaxError "getParams: Expected a function signature")

let rec getFunctionRec (name : string) (function_header:Ast.expr) (function_body : Ast.expr) : Ast.expr list =
  match function_body with
  | Ast.Match (expr, expr_list) -> 
    (match expr_list with
      | Pattern(left, e) :: t ->
        parse (Print.string_of_expr 
          (Substitution.apply (Substitution.singleton (Print.string_of_expr expr) 
          (parse(getCons left))) 
          (parse (name ^ " " ^ getParams function_header))) 
          ^ 
          " = " 
          ^ 
          Print.string_of_expr e) 
          :: getFunctionRec name function_header (Ast.Match(expr, t))
      | [] -> []
      | _ -> raise (SyntaxError "getFunctionRec: Expected a pattern")
    )
  | _ -> raise (SyntaxError "getFunctionRec: Expected a match expression")
  

let produce_output_simple (decls : Ast.declaration list) : string =
  (* Define a nested helper function with an accumulator *)
  let rec helper decls acc =
  match decls with
    | h :: t -> 
      (match h with 
        | Ast.Lemma(name, variables, def, hint_opt) ->
          (let new_acc = acc @ [(("lemma " ^ name), variables, def)] in
            let result = processLemma (name, def, hint_opt) acc ^ "\n" in
              result ^ helper t new_acc)
        | Ast.Type (_,_)->
          failwith "unimplemented"
        | Ast.Function (function_header, function_body)->
        (match function_header with
        | Ast.FunctionSignature (name, _, variables) ->
          let function_records = getFunctionRec name function_header function_body in
          let new_acc_entries = List.map (fun record -> ("definition of " ^ name, Some variables, record)) function_records in
          let acc1 = acc @ new_acc_entries in
          helper t acc1
        | _ -> raise (SyntaxError "Expected a function signature")
        )
      )
    | [] -> ""
              ^ print_lemma acc
  in
  helper decls []

  
  