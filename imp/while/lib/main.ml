open Ast
open Types

let bind st x v = fun y -> if x=y then v else st y
let bottom= fun _ -> failwith "Errore"
let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec eval_expr : state -> expr -> exprval = 
fun st e -> match e with
| True -> Bool true
| False -> Bool false
| Const n -> Nat n
| Var v-> st v
| And (e0, e1) -> (
    match (eval_expr st e0, eval_expr st e1) with
    | Bool e2, Bool e3 -> Bool (e2 && e3)
    | _ -> failwith "Gli argomenti devono essere booleani")
| Or (e0, e1) -> (
    match (eval_expr st e0, eval_expr st e1) with
    | Bool e2, Bool e3 -> Bool (e2 || e3)
    | _ -> failwith "Gli argomenti devono essere booleani")
| Not e0 -> (
    match eval_expr st e0 with
    | Bool e -> if e then Bool false else Bool true
    | _ -> failwith "L' expr di un not deve essere un booleano")
| Sub (e0, e1) -> (
  match (eval_expr st e0, eval_expr st e1) with
  | Nat e2, Nat e3 -> Nat (e2 - e3)
  | _ -> failwith "Gli argomenti devono essere naturali")
| Add (e0, e1) -> (
  match (eval_expr st e0, eval_expr st e1) with
  | Nat e2, Nat e3 -> Nat (e2 + e3)
  | _ -> failwith "Gli argomenti devono essere naturali")
| Mul (e0, e1) -> (
  match (eval_expr st e0, eval_expr st e1) with
  | Nat e2, Nat e3 -> Nat (e2 * e3)
  | _ -> failwith "Gli argomenti devono essere naturali")
| Eq (e0, e1) -> (
  match (eval_expr st e0, eval_expr st e1) with
  | Nat e2, Nat e3 -> if e2=e3 then Bool true else Bool false
  | _ -> failwith "Gli argomenti devono essere naturali")
| Leq (e0, e1) -> (
  match (eval_expr st e0, eval_expr st e1) with
  | Nat e2, Nat e3 -> if e2<=e3 then Bool true else Bool false
  | _ -> failwith "Gli argomenti devono essere naturali")


  let rec trace1 : conf -> conf = function
  | Cmd(Skip,st) -> St(st)
  | Cmd(If(True,c1,_),st)-> Cmd(c1,st)
  | Cmd(If(False, _, c2),st) -> Cmd(c2,st)
  | Cmd(If(e,c1,c2),st) -> (
     match eval_expr st e with 
     | Bool b1-> if b1 then Cmd(c1,st) else Cmd(c2,st)
     | _ -> failwith "Non sono ammessi nat" )
  | Cmd (While (e, c), st) -> (
    match eval_expr st e with
    | Bool true -> Cmd (Seq (c, While (e, c)), st)
    | Bool false -> St st
    | _ -> failwith "Non sono ammessi nat" )
  | Cmd(Assign(x,v),st)-> St (bind st x (eval_expr st v))
  | Cmd(Seq(c1,c2),st) -> (
    match trace1 (Cmd(c1,st)) with 
    | Cmd(c1',st') -> Cmd(Seq(c1',c2),st')
    | St st' -> Cmd(c2,st')
    )
  | _ -> raise NoRuleApplies
  
let trace (n_steps : int) (c : cmd) : conf list= 
 let conf0= Cmd(c,bottom) in 
 let rec helper n conf = 
  if n>0 then
      (try
      let conf' = trace1 conf in 
      conf :: helper (n-1) conf'
      with NoRuleApplies -> [ conf ])
  else
      [ conf ]
  in
  helper n_steps conf0