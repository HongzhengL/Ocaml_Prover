let%test _ = Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "let (*prove*) cf_idempotent (h : int) = (cf (cf h) = cf h)") =
    [Oprovl.Ast.Lemma
  (Oprovl.Ast.FunctionLeft (Oprovl.Ast.Id (Oprovl.Ast.LemmaID "cf_idempotent"),
    Oprovl.Ast.TypeAnotation (Oprovl.Ast.ParamID "h", Oprovl.Ast.TypeID "int")),
  Oprovl.Ast.Bop (Oprovl.Ast.Equal,
   Oprovl.Ast.FunctionRight (Oprovl.Ast.Id (Oprovl.Ast.FuncID "cf"),
    Oprovl.Ast.FunctionRight (Oprovl.Ast.Id (Oprovl.Ast.FuncID "cf"),
     Oprovl.Ast.Id (Oprovl.Ast.ParamID "h"))),
   Oprovl.Ast.FunctionRight (Oprovl.Ast.Id (Oprovl.Ast.FuncID "cf"),
    Oprovl.Ast.Id (Oprovl.Ast.ParamID "h"))))]
 

let%test _ =  Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "let (*prove*) inv_involution (h : int) = (inv (inv h) = h)") = 
    [Oprovl.Ast.Lemma
  (Oprovl.Ast.FunctionLeft
    (Oprovl.Ast.Id (Oprovl.Ast.LemmaID "inv_involution"),
    Oprovl.Ast.TypeAnotation (Oprovl.Ast.ParamID "h", Oprovl.Ast.TypeID "int")),
  Oprovl.Ast.Bop (Oprovl.Ast.Equal,
   Oprovl.Ast.FunctionRight (Oprovl.Ast.Id (Oprovl.Ast.FuncID "inv"),
    Oprovl.Ast.FunctionRight (Oprovl.Ast.Id (Oprovl.Ast.FuncID "inv"),
     Oprovl.Ast.Id (Oprovl.Ast.ParamID "h"))),
   Oprovl.Ast.Id (Oprovl.Ast.ParamID "h")))]

let%test _ = Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "let (*prove*) cf_inv_commute (h : int) = (cf (inv h) = inv (cf h))") = 
    [Oprovl.Ast.Lemma
  (Oprovl.Ast.FunctionLeft
    (Oprovl.Ast.Id (Oprovl.Ast.LemmaID "cf_inv_commute"),
    Oprovl.Ast.TypeAnotation (Oprovl.Ast.ParamID "h", Oprovl.Ast.TypeID "int")),
  Oprovl.Ast.Bop (Oprovl.Ast.Equal,
   Oprovl.Ast.FunctionRight (Oprovl.Ast.Id (Oprovl.Ast.FuncID "cf"),
    Oprovl.Ast.FunctionRight (Oprovl.Ast.Id (Oprovl.Ast.FuncID "inv"),
     Oprovl.Ast.Id (Oprovl.Ast.ParamID "h"))),
   Oprovl.Ast.FunctionRight (Oprovl.Ast.Id (Oprovl.Ast.FuncID "inv"),
    Oprovl.Ast.FunctionRight (Oprovl.Ast.Id (Oprovl.Ast.FuncID "cf"),
     Oprovl.Ast.Id (Oprovl.Ast.ParamID "h")))))]

let%test _ = Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "let (*prove*) cf_inv_property (h : int) = (cf (inv (cf (inv h))) = cf h)") = 
    [Oprovl.Ast.Lemma
  (Oprovl.Ast.FunctionLeft
    (Oprovl.Ast.Id (Oprovl.Ast.LemmaID "cf_inv_property"),
    Oprovl.Ast.TypeAnotation (Oprovl.Ast.ParamID "h", Oprovl.Ast.TypeID "int")),
  Oprovl.Ast.Bop (Oprovl.Ast.Equal,
   Oprovl.Ast.FunctionRight (Oprovl.Ast.Id (Oprovl.Ast.FuncID "cf"),
    Oprovl.Ast.FunctionRight (Oprovl.Ast.Id (Oprovl.Ast.FuncID "inv"),
     Oprovl.Ast.FunctionRight (Oprovl.Ast.Id (Oprovl.Ast.FuncID "cf"),
      Oprovl.Ast.FunctionRight (Oprovl.Ast.Id (Oprovl.Ast.FuncID "inv"),
       Oprovl.Ast.Id (Oprovl.Ast.ParamID "h"))))),
   Oprovl.Ast.FunctionRight (Oprovl.Ast.Id (Oprovl.Ast.FuncID "cf"),
    Oprovl.Ast.Id (Oprovl.Ast.ParamID "h"))))]

(* program defined type tests *)
    
let%test _ = Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "type list = | Nil | Cons") =
        [Oprovl.Ast.Type (Oprovl.Ast.TypeID "list",
  [Oprovl.Ast.Id (Oprovl.Ast.VariantID "Nil");
   Oprovl.Ast.Id (Oprovl.Ast.VariantID "Cons")])]

let%test _ = Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "type list = | Nil | Cons of (int * list)") =
    [Oprovl.Ast.Type (Oprovl.Ast.TypeID "list",
  [Oprovl.Ast.Id (Oprovl.Ast.VariantID "Nil");
   Oprovl.Ast.Variant (Oprovl.Ast.VariantID "Cons",
    Oprovl.Ast.TypeTuple [Oprovl.Ast.TypeID "int"; Oprovl.Ast.TypeID "list"])])]

let%test _ = Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "type list = | Nil | Cons of int * list") =
    [Oprovl.Ast.Type (Oprovl.Ast.TypeID "list",
  [Oprovl.Ast.Id (Oprovl.Ast.VariantID "Nil");
   Oprovl.Ast.Variant (Oprovl.Ast.VariantID "Cons",
    Oprovl.Ast.TypeTuple [Oprovl.Ast.TypeID "int"; Oprovl.Ast.TypeID "list"])])]

(* program defined function tests *)

let%test _ = Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "let rec append (l1 : list) (l2 : list) : list = h") = 
    [Oprovl.Ast.Function
  (Oprovl.Ast.FunctionHeader
    (Oprovl.Ast.TypeAnotation (Oprovl.Ast.FuncID "append",
      Oprovl.Ast.TypeID "list"),
    [Oprovl.Ast.TypeAnotation (Oprovl.Ast.ParamID "l1",
      Oprovl.Ast.TypeID "list");
     Oprovl.Ast.TypeAnotation (Oprovl.Ast.ParamID "l2",
      Oprovl.Ast.TypeID "list")]),
  Oprovl.Ast.Id (Oprovl.Ast.ParamID "h"))]

let %test _ = Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "let rec append l1 l2 : list = h") = 
    [Oprovl.Ast.Function
  (Oprovl.Ast.FunctionHeader
    (Oprovl.Ast.TypeAnotation (Oprovl.Ast.FuncID "append",
      Oprovl.Ast.TypeID "list"),
    [Oprovl.Ast.Id (Oprovl.Ast.ParamID "l1");
     Oprovl.Ast.Id (Oprovl.Ast.ParamID "l2")]),
  Oprovl.Ast.Id (Oprovl.Ast.ParamID "h"))]

let %test _ =  Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "let rec append l1 l2 = h") = 
    [Oprovl.Ast.Function
  (Oprovl.Ast.FunctionHeader (Oprovl.Ast.Id (Oprovl.Ast.FuncID "append"),
    [Oprovl.Ast.Id (Oprovl.Ast.ParamID "l1");
     Oprovl.Ast.Id (Oprovl.Ast.ParamID "l2")]),
  Oprovl.Ast.Id (Oprovl.Ast.ParamID "h"))]

let %test _ = Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "let rec f1 p1 = bool_func (p1 = p1) (p2)") = 
    [Oprovl.Ast.Function
  (Oprovl.Ast.FunctionHeader (Oprovl.Ast.Id (Oprovl.Ast.FuncID "f1"),
    [Oprovl.Ast.Id (Oprovl.Ast.ParamID "p1")]),
  Oprovl.Ast.FunctionRight
   (Oprovl.Ast.FunctionRight (Oprovl.Ast.Id (Oprovl.Ast.FuncID "bool_func"),
     Oprovl.Ast.Bop (Oprovl.Ast.Equal, Oprovl.Ast.Id (Oprovl.Ast.ParamID "p1"),
      Oprovl.Ast.Id (Oprovl.Ast.ParamID "p1"))),
   Oprovl.Ast.Id (Oprovl.Ast.ParamID "p2")))]

let %test _ = Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "let rec f1 p1 = bool_func (p1 = p1) p2") = 
    [Oprovl.Ast.Function
  (Oprovl.Ast.FunctionHeader (Oprovl.Ast.Id (Oprovl.Ast.FuncID "f1"),
    [Oprovl.Ast.Id (Oprovl.Ast.ParamID "p1")]),
  Oprovl.Ast.FunctionRight
   (Oprovl.Ast.FunctionRight (Oprovl.Ast.Id (Oprovl.Ast.FuncID "bool_func"),
     Oprovl.Ast.Bop (Oprovl.Ast.Equal, Oprovl.Ast.Id (Oprovl.Ast.ParamID "p1"),
      Oprovl.Ast.Id (Oprovl.Ast.ParamID "p1"))),
   Oprovl.Ast.Id (Oprovl.Ast.ParamID "p2")))]

let %test _ = Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "let (*prove*) rev_append (l1 : list) (l2 : list) = (reverse (append l1 l2) = append (reverse l2) (reverse l1))") = 
        [Oprovl.Ast.Lemma
  (Oprovl.Ast.FunctionLeft
    (Oprovl.Ast.FunctionLeft (Oprovl.Ast.Id (Oprovl.Ast.LemmaID "rev_append"),
      Oprovl.Ast.TypeAnotation (Oprovl.Ast.ParamID "l1",
       Oprovl.Ast.TypeID "list")),
    Oprovl.Ast.TypeAnotation (Oprovl.Ast.ParamID "l2",
     Oprovl.Ast.TypeID "list")),
  Oprovl.Ast.Bop (Oprovl.Ast.Equal,
   Oprovl.Ast.FunctionRight (Oprovl.Ast.Id (Oprovl.Ast.FuncID "reverse"),
    Oprovl.Ast.FunctionRight
     (Oprovl.Ast.FunctionRight (Oprovl.Ast.Id (Oprovl.Ast.FuncID "append"),
       Oprovl.Ast.Id (Oprovl.Ast.ParamID "l1")),
     Oprovl.Ast.Id (Oprovl.Ast.ParamID "l2"))),
   Oprovl.Ast.FunctionRight
    (Oprovl.Ast.FunctionRight (Oprovl.Ast.Id (Oprovl.Ast.FuncID "append"),
      Oprovl.Ast.FunctionRight (Oprovl.Ast.Id (Oprovl.Ast.FuncID "reverse"),
       Oprovl.Ast.Id (Oprovl.Ast.ParamID "l2"))),
    Oprovl.Ast.FunctionRight (Oprovl.Ast.Id (Oprovl.Ast.FuncID "reverse"),
     Oprovl.Ast.Id (Oprovl.Ast.ParamID "l1")))))]

