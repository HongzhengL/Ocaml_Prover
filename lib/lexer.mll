{
    open Parser
}

let white = [' ' '\t']+
let lowerletter = ['a'-'z']
let upperletter = ['A'-'Z']
let digit = ['0'-'9']
let idbody = lowerletter | upperletter | digit | '_'
let id = lowerletter idbody*
let constructor = upperletter idbody*
let newline = '\r' | '\n' | "\n\r" | "\r\n"

rule read = parse
  | newline { Lexing.new_line lexbuf; read lexbuf}
  | white { read lexbuf }
  | ":" { COLON }
  | "*" { STAR }
  | "=" { EQUAL }
  | "," { COMMA }
  | "(*hint:" { HINT }
  | "axiom" { AXIOM }
  | "induction" { INDUCTION }
  | "(*prove*)" { PROVE }
  | "(*" { comment 0 lexbuf}
  | "*)" { RCOMMENT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "let" { LET }
  | "rec" { REC }
  | "match" { MATCH }
  | "with" { WITH }
  | "type" { TYPE }
  | "|" { BAR }
  | "of" { OF }
  | "->" { ARROW }
  | id { ID (Lexing.lexeme lexbuf) }
  | constructor { CONSTRUCTOR (Lexing.lexeme lexbuf)}
  | eof { EOF }
and comment level = parse
  | "(*" { comment (level + 1) lexbuf}
  | "*)" { if (level = 0) then read lexbuf else comment (level - 1) lexbuf }
  | _ { comment level lexbuf }
  
