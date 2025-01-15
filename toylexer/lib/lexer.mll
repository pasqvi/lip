{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*

let atok = ['A'-'Z'] chr*
let lvowel = ['a' 'e' 'i' 'o' 'u']
let vowel = ['a' 'A' 'e' 'E' 'i' 'I' 'o' 'O' 'u' 'U']
let btok = lvowel+
let cons = letter # vowel
let ctok = cons* vowel? cons*
let digit = ['0'-'9']
let frac = '.' digit*
let dtok = ['-']? digit* frac?
let hexchr = ['0'-'9' 'A'-'F' 'a'-'f']
let etok = '0' ('x'|'X') (hexchr hexchr)+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }
  | atok { ATOK (Lexing.lexeme lexbuf) }
  | btok { BTOK (Lexing.lexeme lexbuf) }
  | ctok { CTOK (Lexing.lexeme lexbuf) }
  | dtok { DTOK (Lexing.lexeme lexbuf) }
  | etok { ETOK (Lexing.lexeme lexbuf) }          
  | eof { EOF }
