module type Substitution = sig
  type expression  (* You need to define this type *)
  
  type t = expression Map.Make(String).t
  val empty : t
  val singleton : string -> expression -> t
  val merge : t -> t -> t option
  val find : string -> t -> expression option
end

(* module Substitution = struct
  module MM = Map.Make(String)
  type expression  (* This should be defined to represent your expressions *)
  
  type t = expression MM.t

  let empty = MM.empty

  let singleton key expr = MM.singleton key expr

  let merge s1 s2 =
    MM.merge (fun key expr1_opt expr2_opt ->
      match expr1_opt, expr2_opt with
      | Some expr1, Some expr2 -> 
          (* Define logic to handle conflicts here. Return None if conflict *)
          ...
      | Some expr, None | None, Some expr -> Some expr
      | None, None -> None
    ) s1 s2

  let find key s = MM.find_opt key s
end *)

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

(* let attemptRewrite (variables : string list) (equality : Ast.expr) (expr : Ast.expr) : Ast.expr option =
  failwith "unimplemented" *)

(* 
 * tryEqualities : expression -> (string * string list * equality) list -> (string * expression) option
 *   Given an expression, tries to apply each equality in the list. Returns the name of
 *   the rule that was successfully applied and the resulting expression   
*)
(* let tryEqualities (expr : Ast.expr) (equalities : (string * string list * Ast.expr) list) : (string * Ast.expr) option =
  failwith "unimplemented"

let performSteps (expr : Ast.expr) (equalities : (string * string list * Ast.expr) list) : (string * Ast.expr) list =
  failwith "unimplemented"

let produceProof (equality : Ast.expr) (equalities : (string * string list * Ast.expr) list) : string list =
  failwith "unimplemented" *)

let processLemma (name, (def : Ast.expr), (hint_opt : Ast.hint option))=
  match hint_opt with
  | Some Axiom -> 
      (match def with
      | Ast.Equal (e1, e2) -> 
        name ^ " is assumed as an axiom:\n" ^ Print.string_of_expr e1 
        ^ "\n= {axiom}\n" ^ Print.string_of_expr e2 ^ "\n"
      | _ -> failwith "Expected an equality, but " ^ Print.string_of_expr def ^ " was given")
  | Some (Induction _) -> failwith "unimplemented"
  | None -> failwith "unimplemented"
    (* (match tryEqualities with
      | Some (s, e) -> Some (s, e)
      | None -> None) *)

(* let processType (decl : Ast.declaration) =
  failwith "unimplemented"

let processFunction (decl : Ast.declaration) =
  failwith "unimplemented" *)

(* let rec produce_output_simple (decls : Ast.declaration list) : string =
  match decls with
  | h :: t -> (match h with 
    | Ast.Lemma(name, _, def, hint_opt) ->
      processLemma (name, def, hint_opt) ^ "\n" ^ produce_output_simple t
    | Ast.Type (_,_)->
      failwith "unimplemented"
      (* processType h ^ "\n" ^ produce_output_simple t *)
    | Ast.Function (_,_)->
      failwith "unimplemented"
      (* processFunction h ^ "\n" ^ produce_output_simple t *)
  )
  | [] -> "" *)

let produce_output_simple (decls : Ast.declaration list) : string =
  (* Define a nested helper function with an accumulator *)
  let rec helper decls acc =
  match decls with
    | h :: t -> 
      (match h with 
        | Ast.Lemma(name, _, def, hint_opt) ->
          let new_acc = (("lemma " ^ name), def)::acc in
            let result = processLemma (name, def, hint_opt) ^ "\n" in
              result ^ helper t new_acc
        | Ast.Type (_,_)->
          failwith "unimplemented"
          (* processType h ^ "\n" ^ helper t acc *)
        | Ast.Function (_,_)->
          failwith "unimplemented"
          (* processFunction h ^ "\n" ^ helper t acc *)
      )
    | [] -> ""(* Combine the results from acc here *)
  in
  helper decls []

  
  