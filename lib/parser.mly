(* 
   this is the header. it contains valid ocaml code that will be
   run at the begining of the paser. Here we put open Ast because
   there will be a module called Ast and we dont want do have to
   prefix everything with Ast.*
*)
%{
    open Ast
%}

(* 
    this is the declaration section. Contains definitions for out
    tokens
*)
%token <string> ID
%token EOF
%token COLON
%token BAR
%token ARROW
%token EQUAL
%token LPAREN
%token RPAREN
%token PROVE
%token LET
%token REC
%token MATCH
%token WITH
%token TYPE
%token OF

(*
    operator associativity and precidence. Lowest precidence is
    first line, then highest precidence is last line.
*)
%left EQUAL
(* start with a rule named "prog" *)
%start <Ast.declaration list> prog
%%

(* rules section. Syntax represents BNF *)
prog:
    | d1 = declaration; cont = prog { d1::cont }
    | d = declaration; EOF { d::[] }
    ;

declaration:
    | LET; PROVE; e = function_l; EQUAL; def = expr 
      { Proof(e, def) }
 
function_l:
  | f = function_l; tap = type_annot {FunctionLeft (f, tap)}
  | name = ID {Id (LemmaID name)}

function_r:
  | func = ID; fr = function_r {FunctionRight (FuncID func, fr)}
  | name = ID {Id (ParamID name)}
  | LPAREN; fr = function_r; RPAREN; { fr }

type_annot:
  | param = ID; COLON; t = ID {TypeAnotation(ParamID param, TypeID t)}
  | LPAREN; ta = type_annot; RPAREN { ta }

expr:
  | param = ID; {Id (ParamID param)}
  | e1 = expr; EQUAL; e2 = expr {Bop(Equal, e1, e2)}
  | func = ID; fr = function_r; { FunctionRight( FuncID func, fr) }
  | LPAREN; e = expr; RPAREN; { e }
  ;




