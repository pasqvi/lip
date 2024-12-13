open Types

(* Use this grammar record as a blueprint for the exercises. *)
let todo : grammar =
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [              (* Indexes *)
        S --> "0S0"; (* 0 *)
        S --> "1S1"; (* 1 *)
        S --> "";    (* 2 *)
      ];
    start = S;
  }


(* #### Exercise 1, easy (zero_n_one_n) *)
let zero_n_one_n : grammar = 
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [              (* Indexes *)
        S --> "0S1"; (* 0 *)
        S --> "";    (* 1 *)
      ];
    start = S;
  }


(* #### Exercise 2, easy (palindromes) *)
let palindromes : grammar = 
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [              (* Indexes *)
        S --> "0S0"; (* 0 *)
        S --> "1S1"; (* 1 *)
        S --> "1";   (* 2 *)
        S --> "0";   (* 3 *)
        S --> "";    (* 4 *)
      ];
    start = S;
  }


(* #### Exercise 3, medium (balanced_parentheses) *)
let balanced_parentheses : grammar = 
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [              (* Indexes *)
      S --> "(S)"; (* 0 *)
      S --> "[S]"; (* 1 *)
      S --> "{S}"; (* 2 *)
      S --> "()S"; (* 3 *)
      S --> "[]S"; (* 4 *)
      S --> "{}S"; (* 5 *)
      S --> "S()"; (* 6 *)
      S --> "S[]"; (* 7 *)
      S --> "S{}"; (* 8 *)
      S --> "";    (* 9 *)
      ];
    start = S;
  }


(* #### Exercise 4, hard (same_amount)

   Hint: model the language of words where the number of 0's is
   one greater than the number of 1's and viceversa, then combine them.
*)
let same_amount : grammar = 
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [              (* Indexes *)
        S --> "0S1"; (* 0 *)
        S --> "01S"; (* 1 *)
        S --> "S01"; (* 2 *)
        S --> "10S"; (* 3 *)
        S --> "1S0"; (* 4 *)
        S --> "S10"; (* 5 *)
        S --> "";    (* 6 *)
      ];
    start = S;
  }
