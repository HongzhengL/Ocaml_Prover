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
%token LPAREN
%token RPAREN
%token LCOMMENT
%token RCOMMENT
%token PROVE


(*
    operator associativity and precidence. Lowest precidence is
    first line, then highest precidence is last line.
*)

(* start with a rule named "prog" *)
%start <Ast.expr> prog
%%

(* rules section. Syntax represents BNF *)
prog:
    | e = expr; EOF { e }
    ;

expr:
  | x = ID { Var x }
  | name = ID; COLON; type_name = TYPENAME { TypeDecl(type_name, name) } 
  | LPAREN; e = expr; RPAREN; { e }
  ;







