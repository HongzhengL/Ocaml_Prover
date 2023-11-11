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
%token STAR
%token COMMA
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
      {Lemma(e, def)}
    | LET; REC; fh = function_header; EQUAL; def = expr 
      {Function(fh, def)}
    | TYPE; type_name = ID; EQUAL; vl = variant_list
      {Type(TypeID type_name, vl)}
 
function_header:
    | func_name = ID; params = parameter_list 
      {FunctionHeader(Id(FuncID(func_name)), params)}
    | func_name = ID; params = parameter_list; COLON; type_name = ID
      {FunctionHeader(TypeAnotation(FuncID(func_name), TypeID(type_name)), params)}

(* Restriction on parameter type annotations: must be in parentheses*)
parameter_list:
    | param_name = ID; param_cont = parameter_list; 
        {Id(ParamID(param_name))::param_cont}
    | param_name = ID; 
        {Id(ParamID(param_name))::[]}
    | LPAREN; param_ta = type_annot; RPAREN; param_cont = parameter_list; 
        {param_ta::param_cont}
    | LPAREN; param_ta = type_annot; RPAREN 
        {param_ta::[]}

function_l:
  | f = function_l; tap = type_annot 
    {FunctionLeft (f, tap)}
  | name = ID 
    {Id (LemmaID name)}

function_r:
  (* functions can accept expressions as arguments only
     if the are in parentheses *)
  | func_name = ID; LPAREN; e = expr; RPAREN;
    {FunctionRight(Id(FuncID func_name), e)}
  | func = function_r; LPAREN; e = expr; RPAREN;
    {FunctionRight(func, e)}
  (* otherwise they must be parameter identifications *)
  | func_name = ID; param_name = ID;
    {FunctionRight(Id(FuncID func_name), Id(ParamID param_name))}
  | func = function_r; param_name = ID
    {FunctionRight(func, Id(ParamID param_name))}

variant_list:
  | BAR; variant_name = ID; next_v = variant_list 
    {Id(VariantID variant_name)::next_v}
  | BAR; variant_name = ID; OF; tt = tuple_type; next_v = variant_list
    {Variant(VariantID(variant_name), TypeTuple(tt))::next_v}
  | BAR; variant_name = ID; 
    {Id(VariantID variant_name)::[] }
  | BAR; variant_name = ID; OF; tt = tuple_type;
    {Variant(VariantID(variant_name), TypeTuple(tt))::[]}

tuple_type:
  | type_name = ID; STAR; type_cont = tuple_type 
    {TypeID (type_name)::type_cont} 
  | type_name = ID 
    {TypeID (type_name)::[]} 
  | LPAREN; tt = tuple_type; RPAREN 
    {tt}
  
type_annot:
  | param = ID; COLON; t = ID 
    {TypeAnotation(ParamID param, TypeID t)}
  | LPAREN; ta = type_annot; RPAREN 
    {ta}

expr:
  | param = ID; 
    {Id (ParamID param)}
  | e1 = expr; EQUAL; e2 = expr 
    {Bop(Equal, e1, e2)}
  | fr = function_r; 
    {fr}
  | LPAREN; e = expr; RPAREN; 
    {e}
  ;




