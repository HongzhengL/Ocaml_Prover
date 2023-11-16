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
%token <string> CONSTRUCTOR
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
%token AXIOM
%token INDUCTION
%token HINT
%token RCOMMENT

(*
    operator associativity and precidence. Lowest precidence is
    first line, then highest precidence is last line.
*)
%nonassoc ARROW
%nonassoc BAR
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
    | LET; PROVE; name = ID; vl = option(variable_list); EQUAL; def = expr; h = option(hint)
      {Lemma(name, vl, def, h) }
    | LET; REC; fh = function_header; EQUAL; def = expr 
      {RecFunction(fh, def)}
    | TYPE; type_name = ID; EQUAL; vl = type_list
      {Type(type_name, vl)}

hint :
    | HINT; AXIOM; RCOMMENT{ Axiom }
    | HINT; INDUCTION; variable = ID; RCOMMENT { Induction variable }
 
function_header:
    | func_name = ID; params = parameter_list; COLON; type_name = ID
      {FunctionHeader(func_name, Some type_name, params)}
    | func_name = ID; params = parameter_list
      {FunctionHeader(func_name, None, params)}

(* Restriction on parameter type annotations: must be in parentheses*)
parameter_list:
    | param_name = ID; param_cont = parameter_list; 
        {Id( param_name )::param_cont}
    | param_name = ID; 
        {Id(param_name)::[]}
    | LPAREN; param_ta = type_annot; RPAREN; param_cont = parameter_list; 
        {param_ta::param_cont}
    | LPAREN; param_ta = type_annot; RPAREN 
        {param_ta::[]}

variable_list:
  | f = variable_list; tap = type_annot 
    {f @ [tap]}
  | tap = type_annot 
    {[tap]}

function_r:
  (* functions can accept expressions as arguments only
     if the are in parentheses *)
  | func_name = ID; LPAREN; e = expr; RPAREN;
    {Function(Id(func_name), e)}
  | func = function_r; LPAREN; e = expr; RPAREN;
    {Function(func, e)}
  (* otherwise they must be parameter identifications *)
  | func_name = ID; param_name = ID;
    {Function(Id(func_name), Id(param_name))}
  | func = function_r; param_name = ID
    {Function(func, Id(param_name))}
  | func_name = ID; param_name = CONSTRUCTOR;
    {Function(Id(func_name), Constructor(param_name, None))}
  | func = function_r; param_name = CONSTRUCTOR
    {Function(func, Constructor(param_name, None))}

type_list:
  | BAR; variant_name = CONSTRUCTOR; next_v = type_list 
    {Constructor(variant_name, None)::next_v}
  | BAR; variant_name = CONSTRUCTOR; OF; tt = tuple_type; next_v = type_list
    {Constructor(variant_name, Some tt)::next_v}
  | BAR; variant_name = CONSTRUCTOR; 
    {Constructor(variant_name, None)::[]}
  | BAR; variant_name = CONSTRUCTOR; OF; tt = tuple_type;
    {Constructor(variant_name, Some tt)::[]}

tuple_type:
  | type_name = ID; STAR; type_cont = tuple_type 
    {Id(type_name)::type_cont} 
  | type_name = ID 
    {Id(type_name)::[]} 
  | LPAREN; tt = tuple_type; RPAREN 
    {tt}
  
type_annot:
  | param = ID; COLON; t = ID 
    {TypeAnotation(param, t)}
  | LPAREN; ta = type_annot; RPAREN 
    {ta}

pattern_list:
  | c = constructor_ta; ARROW; e = expr; BAR; pl_cont = pattern_list; 
    {Pattern(c, e)::pl_cont}
  | c = constructor_ta; ARROW; e = expr; 
    {Pattern(c, e)::[]}

constructor:
  | constructor_name = CONSTRUCTOR;
    {Constructor(constructor_name, None)}
  | constructor_name = CONSTRUCTOR; LPAREN; et = expr_tuple; RPAREN;
   {Constructor(constructor_name, Some et)}

constructor_ta:
  | constructor_name = CONSTRUCTOR;
    {Constructor(constructor_name, None)}
  | constructor_name = CONSTRUCTOR; tat = type_annot_tuple;
   {Constructor(constructor_name, Some tat )}

expr_tuple:
  | e = expr; COMMA; et = expr_tuple
    {e::et}
  | e = expr; 
    {e::[]}

type_annot_tuple:
  | LPAREN; ta = type_annot; tat_cont = type_annot_tuple
    {ta::tat_cont}
  | COMMA; ta = type_annot; tat_cont = type_annot_tuple
    {ta::tat_cont}
  | RPAREN;
    {[]}

expr:
  | MATCH; e = expr; WITH; BAR; pl = pattern_list;
    {Match(e, pl)}
  | param = ID; 
    {Id (param)}
  | e1 = expr; EQUAL; e2 = expr 
    {Equal(e1, e2)}
  | fr = function_r; 
    {fr}
  | con = constructor
    {con}
  | LPAREN; e = expr; RPAREN; 
    {e}
  ;




