let%test _ = Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "let (*prove*) cf_idempotent (h : int) = (cf (cf h) = cf h)") =
    [Oprovl.Ast.Lemma ("cf_idempotent", [Oprovl.Ast.TypeAnotation ("h", "int")],
  Oprovl.Ast.Equal
   (Oprovl.Ast.Function (Oprovl.Ast.Id "cf",
     Oprovl.Ast.Function (Oprovl.Ast.Id "cf", Oprovl.Ast.Id "h")),
   Oprovl.Ast.Function (Oprovl.Ast.Id "cf", Oprovl.Ast.Id "h")))]

let%test _ =  Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "let (*prove*) inv_involution (h : int) = (inv (inv h) = h)") = 
    [Oprovl.Ast.Lemma ("inv_involution", [Oprovl.Ast.TypeAnotation ("h", "int")],
  Oprovl.Ast.Equal
   (Oprovl.Ast.Function (Oprovl.Ast.Id "inv",
     Oprovl.Ast.Function (Oprovl.Ast.Id "inv", Oprovl.Ast.Id "h")),
   Oprovl.Ast.Id "h"))]

let%test _ = Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "let (*prove*) cf_inv_commute (h : int) = (cf (inv h) = inv (cf h))") = 
    [Oprovl.Ast.Lemma ("cf_inv_commute", [Oprovl.Ast.TypeAnotation ("h", "int")],
  Oprovl.Ast.Equal
   (Oprovl.Ast.Function (Oprovl.Ast.Id "cf",
     Oprovl.Ast.Function (Oprovl.Ast.Id "inv", Oprovl.Ast.Id "h")),
   Oprovl.Ast.Function (Oprovl.Ast.Id "inv",
    Oprovl.Ast.Function (Oprovl.Ast.Id "cf", Oprovl.Ast.Id "h"))))]

let%test _ = Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "let (*prove*) cf_inv_property (h : int) = (cf (inv (cf (inv h))) = cf h)") = 
    [Oprovl.Ast.Lemma ("cf_inv_property", [Oprovl.Ast.TypeAnotation ("h", "int")],
  Oprovl.Ast.Equal
   (Oprovl.Ast.Function (Oprovl.Ast.Id "cf",
     Oprovl.Ast.Function (Oprovl.Ast.Id "inv",
      Oprovl.Ast.Function (Oprovl.Ast.Id "cf",
       Oprovl.Ast.Function (Oprovl.Ast.Id "inv", Oprovl.Ast.Id "h")))),
   Oprovl.Ast.Function (Oprovl.Ast.Id "cf", Oprovl.Ast.Id "h")))]

(* program defined type tests *)
    
let%test _ = Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "type list = | Nil | Cons") =
    [Oprovl.Ast.Type ("list",
  [Oprovl.Ast.Constructor ("Nil", None); Oprovl.Ast.Constructor ("Cons", None)])]

let%test _ = Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "type list = | Nil | Cons of (int * list)") =
    [Oprovl.Ast.Type ("list",
  [Oprovl.Ast.Constructor ("Nil", None);
   Oprovl.Ast.Constructor ("Cons",
    Some (Oprovl.Ast.ExprTuple [Oprovl.Ast.Id "int"; Oprovl.Ast.Id "list"]))])]

let%test _ = Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "type list = | Nil | Cons of int * list") =
    [Oprovl.Ast.Type ("list",
  [Oprovl.Ast.Constructor ("Nil", None);
   Oprovl.Ast.Constructor ("Cons",
    Some (Oprovl.Ast.ExprTuple [Oprovl.Ast.Id "int"; Oprovl.Ast.Id "list"]))])]

let %test _ = Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "let (*prove*) rev_append (l1 : list) (l2 : list) = (reverse (append l1 l2) = append (reverse l2) (reverse l1))") = 
    [Oprovl.Ast.Lemma ("rev_append",
  [Oprovl.Ast.TypeAnotation ("l1", "list");
   Oprovl.Ast.TypeAnotation ("l2", "list")],
  Oprovl.Ast.Equal
   (Oprovl.Ast.Function (Oprovl.Ast.Id "reverse",
     Oprovl.Ast.Function
      (Oprovl.Ast.Function (Oprovl.Ast.Id "append", Oprovl.Ast.Id "l1"),
      Oprovl.Ast.Id "l2")),
   Oprovl.Ast.Function
    (Oprovl.Ast.Function (Oprovl.Ast.Id "append",
      Oprovl.Ast.Function (Oprovl.Ast.Id "reverse", Oprovl.Ast.Id "l2")),
    Oprovl.Ast.Function (Oprovl.Ast.Id "reverse", Oprovl.Ast.Id "l1"))))]

