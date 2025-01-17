{
open Parser
}

let white = [' ' '\t']+
let const = ['0'-'9']['0'-'9']*
let var = ['a'-'z']['a'-'z']*

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "or" { OR }
  | "and" { AND }
  | "not" { NOT }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "=" { EQ }
  | "<=" { LEQ }
  | "while" { WHILE }
  | "do" { DO }
  | ":=" { ASSIGN }
  | ";" { SEQ }
  | "skip" { SKIP }
  | const { CONST (Lexing.lexeme lexbuf) }
  | var { VAR ( Lexing.lexeme lexbuf ) }
  | eof { EOF }