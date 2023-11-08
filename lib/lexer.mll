{
    open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'A' '_']
let id = letter+
let newline = '\r' | '\n' | "\n\r" | "\r\n"

rule read = parse
  | newline { Lexing.new_line lexbuf; read lexbuf}
  | white { read lexbuf }
  | "int" { TYPENAME (Ast.Int) }
  | ":" { COLON }
  | "=" { EQUAL }
  | "(*prove*)" { PROVE }
  | "(*" { comment lexbuf}
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "let" { LET }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
and comment = parse
  | "*)" { read lexbuf }
  | _ { comment lexbuf }
