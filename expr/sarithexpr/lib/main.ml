open Ast

exception TypeError of string;;

let rec is_nv = function 
    Zero -> true
  | Succ n -> is_nv n 
  | _ -> false 

let string_of_type = function
 | BoolT ->"Bool"
 | NatT -> "Nat"

let rec typecheck = function
 True  -> BoolT
| False -> BoolT
| Zero -> NatT
| Not e ->  (
  match (typecheck e) with
  |  BoolT -> BoolT
  | _ -> raise (TypeError "La NOT è solo su un BoolT") )
| And (e1, e2) ->  (
  match (typecheck e1, typecheck e2) with
  |  BoolT , BoolT -> BoolT
  | _ -> raise (TypeError "La AND è fra due BoolT") )
| Or (e1, e2) -> (
  match (typecheck e1, typecheck e2) with
  |  BoolT , BoolT -> BoolT
  | _ -> raise (TypeError "La OR è fra due BoolT") )
| If (e0, e1, e2) -> (
  match (typecheck e0,typecheck e1,typecheck e2) with
  | BoolT , NatT , NatT -> NatT
  | BoolT , BoolT , BoolT -> BoolT
  |  _ -> raise (TypeError "La then e la else devono avere res dello stesso tipo e condizione BoolT"))
| IsZero e0 ->(
  match (typecheck e0) with
  |  NatT -> BoolT
  | _ -> raise (TypeError "La isZero va fatta su un NatT") )
| Pred e0 ->  (
  match (typecheck e0) with
  |  NatT when e0 <> Zero -> NatT
  | _ -> raise (TypeError "La pred va fatta su un NatT") )
| Succ e0 -> (
  match (typecheck e0) with
  |  NatT -> NatT
  | _ -> raise (TypeError "La succ va fatta su un NatT") )


let rec string_of_expr = function
  | True -> "True"
  | False -> "False"
  | Zero -> "0"
  | Not e -> "Not(" ^ string_of_expr e ^ ")"
  | And (e1, e2) ->
      "And(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"
  | Or (e1, e2) ->
      "Or(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"
  | If (e0, e1, e2) ->
      "If(" ^ string_of_expr e0 ^ "," ^ string_of_expr e1 ^ ","
      ^ string_of_expr e2 ^ ")"
  | IsZero e0 -> "IsZero(" ^ string_of_expr e0 ^ ")"
  | Pred e0 -> "Pred(" ^ string_of_expr e0 ^ ")"
  | Succ e0 -> "Succ(" ^ string_of_expr e0 ^ ")"


let string_of_val = function 
  | Bool true -> "true"
  | Bool false -> "false"
  | Nat n -> string_of_int(n)
 
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies

let rec trace1 = function
  | If (True, e1, _) -> e1
  | If (False, _, e2) -> e2
  | If (e0, e1, e2) -> If (trace1 e0, e1, e2)
  | And (True, e1) -> e1
  | And (False, _) -> False
  | And (e0, e1) -> And (trace1 e0, e1)
  | Or (True, _) -> True
  | Or (False, e1) -> e1
  | Or (e0, e1) -> Or (trace1 e0, e1)
  | Not True -> False
  | Not False -> True
  | Not e0 -> Not (trace1 e0)
  | Succ e -> Succ (trace1 e)
  | Pred(Succ(e0))->  e0
  | Pred(e0) -> Pred (trace1 e0)
  | IsZero(Zero)-> True
  | IsZero(Succ(_) )-> False 
  | IsZero(e0) -> IsZero(trace1 e0)
  | _ -> raise NoRuleApplies

let rec trace e =
  try
    let e' = trace1 e in
    e :: trace e'
  with NoRuleApplies -> [ e ]

let rec eval = function
  | True -> Bool true
  | False -> Bool false
  | If (e0, e1, e2) -> (
      match eval e0 with
      | Nat _ -> failwith "La condizione di un if deve essere un booleano"
      | Bool e -> if e then eval e1 else eval e2)
  | And (e0, e1) -> (
      match (eval e0, eval e1) with
      | Bool e2, Bool e3 -> Bool (e2 && e3)
      | _ -> failwith "Gli argomenti devono essere booleani")
  | Or (e0, e1) -> (
      match (eval e0, eval e1) with
      | Bool e2, Bool e3 -> Bool (e2 || e3)
      | _ -> failwith "Gli argomenti devono essere booleani")
  | Not e0 -> (
      match eval e0 with
      | Bool e -> if e then Bool false else Bool true
      | _ -> failwith "L' expr di un not deve essereun booleano")
  | Zero -> Nat 0
  | Succ e0 -> (
      match eval e0 with
      | Nat n -> Nat (n + 1)
      | Bool _ -> failwith "Non esiste successivo dei booleani")
  | Pred e0 -> (
      match eval e0 with
      | Nat n when n > 0 -> Nat (n - 1)
      | _ -> failwith "Non esiste predecessore dei booleani")
  | IsZero e0 -> (
      match eval e0 with
      | Nat n -> if n=0 then Bool true else Bool false
      | _ -> failwith "Non può essere 0 un booleano")