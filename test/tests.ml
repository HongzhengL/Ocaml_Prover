let%test _ = Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "let (*prove*) cf_idempotent (h : int) = (cf (cf h) = cf h)") =
    [Oprovl.Ast.Proof
  (Oprovl.Ast.FunctionLeft (Oprovl.Ast.Id (Oprovl.Ast.LemmaID "cf_idempotent"),
    Oprovl.Ast.TypeAnotation (Oprovl.Ast.ParamID "h", Oprovl.Ast.TypeID "int")),
  Oprovl.Ast.Bop (Oprovl.Ast.Equal,
   Oprovl.Ast.FunctionRight (Oprovl.Ast.FuncID "cf",
    Oprovl.Ast.FunctionRight (Oprovl.Ast.FuncID "cf",
     Oprovl.Ast.Id (Oprovl.Ast.ParamID "h"))),
   Oprovl.Ast.FunctionRight (Oprovl.Ast.FuncID "cf",
    Oprovl.Ast.Id (Oprovl.Ast.ParamID "h"))))]

let%test _ =  Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "let (*prove*) inv_involution (h : int) = (inv (inv h) = h)") = 
    [Oprovl.Ast.Proof
  (Oprovl.Ast.FunctionLeft
    (Oprovl.Ast.Id (Oprovl.Ast.LemmaID "inv_involution"),
    Oprovl.Ast.TypeAnotation (Oprovl.Ast.ParamID "h", Oprovl.Ast.TypeID "int")),
  Oprovl.Ast.Bop (Oprovl.Ast.Equal,
   Oprovl.Ast.FunctionRight (Oprovl.Ast.FuncID "inv",
    Oprovl.Ast.FunctionRight (Oprovl.Ast.FuncID "inv",
     Oprovl.Ast.Id (Oprovl.Ast.ParamID "h"))),
   Oprovl.Ast.Id (Oprovl.Ast.ParamID "h")))]

let%test _ = Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "let (*prove*) cf_inv_commute (h : int) = (cf (inv h) = inv (cf h))") = 
    [Oprovl.Ast.Proof
  (Oprovl.Ast.FunctionLeft
    (Oprovl.Ast.Id (Oprovl.Ast.LemmaID "cf_inv_commute"),
    Oprovl.Ast.TypeAnotation (Oprovl.Ast.ParamID "h", Oprovl.Ast.TypeID "int")),
  Oprovl.Ast.Bop (Oprovl.Ast.Equal,
   Oprovl.Ast.FunctionRight (Oprovl.Ast.FuncID "cf",
    Oprovl.Ast.FunctionRight (Oprovl.Ast.FuncID "inv",
     Oprovl.Ast.Id (Oprovl.Ast.ParamID "h"))),
   Oprovl.Ast.FunctionRight (Oprovl.Ast.FuncID "inv",
    Oprovl.Ast.FunctionRight (Oprovl.Ast.FuncID "cf",
     Oprovl.Ast.Id (Oprovl.Ast.ParamID "h")))))]

let%test _ = Oprovl.Parser.prog Oprovl.Lexer.read (Lexing.from_string "let (*prove*) cf_inv_property (h : int) = (cf (inv (cf (inv h))) = cf h)") = 
    [Oprovl.Ast.Proof
  (Oprovl.Ast.FunctionLeft
    (Oprovl.Ast.Id (Oprovl.Ast.LemmaID "cf_inv_property"),
    Oprovl.Ast.TypeAnotation (Oprovl.Ast.ParamID "h", Oprovl.Ast.TypeID "int")),
  Oprovl.Ast.Bop (Oprovl.Ast.Equal,
   Oprovl.Ast.FunctionRight (Oprovl.Ast.FuncID "cf",
    Oprovl.Ast.FunctionRight (Oprovl.Ast.FuncID "inv",
     Oprovl.Ast.FunctionRight (Oprovl.Ast.FuncID "cf",
      Oprovl.Ast.FunctionRight (Oprovl.Ast.FuncID "inv",
       Oprovl.Ast.Id (Oprovl.Ast.ParamID "h"))))),
   Oprovl.Ast.FunctionRight (Oprovl.Ast.FuncID "cf",
    Oprovl.Ast.Id (Oprovl.Ast.ParamID "h"))))]
