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
%token <Ast.type_name> TYPENAME
%token EOF
%token COLON
%token EQUAL
%token LPAREN
%token RPAREN
%token PROVE
%token LET

(*
    operator associativity and precidence. Lowest precidence is
    first line, then highest precidence is last line.
*)
%nonassoc EQUAL
(* start with a rule named "prog" *)
%start <Ast.declaration> prog
%%

(* rules section. Syntax represents BNF *)
prog:
    | d = declaration; EOF { d }
    ;

declaration:
    | LET; PROVE; e = function_l; EQUAL; def = expr { Proof(e, def) }

function_l:
  | f = function_l; name = ID { FunctionHeader (f, Identifyer (name)) }
  | f = function_l; 
    LPAREN; id = ID; COLON; t = TYPENAME; RPAREN 
      { FunctionHeader (f, TypeAnotation(t, Identifyer(id))) }
  | name = ID {Identifyer (name) }

function_r:
  | name = ID; f = function_r { FunctionEval (Identifyer (name), f) } 
  | name = ID; { Identifyer (name) }
  | LPAREN; f = function_r; RPAREN; { f }


expr:
  | x = ID { Identifyer (x) }
  | e1 = expr; EQUAL; e2 = expr {Bop(Equal, e1, e2)}
  | e = function_r { e }
  | LPAREN; e = expr; RPAREN; { e }
  ;




