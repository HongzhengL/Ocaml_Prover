{
    open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'A']
let id = letter+

rule read =
  parse
  | white { read lexbuf }
  | "int" { TYPENAME (Ast.Int) }
  | "string" { TYPENAME (Ast.String)} 
  | ":" { COLON }
  | "(*prove*)" { PROVE }
  | "(*" { LCOMMENT }
  | "*)" { RCOMMENT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
