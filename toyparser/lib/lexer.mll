{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let enum = "0x"['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f']* 

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "*" { MUL }
  | "/" { DIV }
  | "+" { PLUS }
  | "-" { MINUS }
  | num { CONST (Lexing.lexeme lexbuf) }
  | enum { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
  