(* Question 1 *)

(* TODO: Write a good set of tests for {!q1a_nat_of_int}. *)
let q1a_nat_of_int_tests : (int * nat) list =  [
  (0, Z);
  (1, S Z);
  (2, S (S Z));
  (5, S (S (S (S (S Z)))));
]

(* TODO:  Implement {!q1a_nat_of_int} using a tail-recursive helper. *)
let q1a_nat_of_int n =
  let rec helper acc n =
    if n <= 0 then acc
    else helper (S acc) (n - 1)
  in
  helper Z n


(* TODO: Write a good set of tests for {!q1b_int_of_nat}. *)
let q1b_int_of_nat_tests : (nat * int) list = [
  (Z, 0);
  (S Z, 1);
  (S (S Z), 2);
  (S (S (S (S (S Z)))), 5);
]

(* TODO:  Implement {!q1b_int_of_nat} using a tail-recursive helper. *)
let q1b_int_of_nat nat =
  let rec helper acc = function
    | Z -> acc
    | S n -> helper (acc + 1) n
  in
  helper 0 nat
(* TODO: Write a good set of tests for {!q1c_add}. *)
let q1c_add_tests : ((nat * nat) * nat) list = [
  ((Z, Z), Z);
  ((Z, S Z), S Z);
  ((S Z, S Z), S (S Z));
  ((S (S Z), S(S Z)), S(S (S (S Z))));
]

(* TODO: Implement {!q1c_add}. *)
let rec q1c_add nat1 nat2 =
  match nat2 with
  | Z -> nat1
  | S n -> q1c_add (S nat1) n

(* Question 2 *)

(* TODO: Implement {!q2a_neg}. *)
let q2a_neg (e : exp) : exp = 
  Times (Const (-1.0), e)
(* TODO: Implement {!q2b_minus}. *)
let q2b_minus (e1 : exp) (e2 : exp) : exp = 
  Plus (e1, q2a_neg e2)
(* TODO: Implement {!q2c_pow}. *)
let rec q2c_pow (e1 : exp) (p : nat) : exp =  
  match p with
  | Z -> Const 1.0
  | S n -> Times (e1, q2c_pow e1 n)

(* Question 3 *)

(* TODO: Write a good set of tests for {!eval}. *)
let eval_tests : ((float * exp) * float) list = [
  ((0.0, Const 1.0), 1.0);
  ((34.9, Var), 34.9);
  ((9.0, Plus (Var, Const 2.0)), 11.0);
  ((2.0, Times (Var, Var)), 4.0);
  ((4.0, Div (Const 8.0, Var)), 2.0);
  ((4.0, Div (Const 10.0, Var)), 2.5);
  ((3.0, Div(Const 10.5,Plus (Times (Var, Const 2.0), Const 1.0))), 1.5);
  ((3.0, Plus (Times (Var, Const 2.0), Const 1.6)), 7.6)]

(* TODO: Implement {!eval}. *)
let rec eval (a : float) (e : exp) : float = 
  match e with
  | Const f -> f
  | Var -> a
  | Plus (e1, e2) -> eval a e1 +. eval a e2
  | Times (e1, e2) -> eval a e1 *. eval a e2
  | Div (e1, e2) -> eval a e1 /. eval a e2


(* Question 4 *)

(* TODO: Write a good set of tests for {!diff_tests}. *)
let diff_tests : (exp * exp) list = [] 
let diff_tests : (exp * exp) list = [
  (Const 5.0, Const 0.0);
  (Var, Const 1.0);
  (Plus (Var, Const 2.0), Plus (Const 1.0, Const 0.0));
  (Times(Var, Const 2.0),(Plus (Times (Const 1., Const 2.), Times (Var, Const 0.)))); 
  (Div (Var, Const 2.0), (Div (Plus (Times (Const 1., Const 2.), Times (Const (-1.), Times (Var, Const 0.))),
                               Times (Const 2., Const 2.))));   
]

(* TODO: Implement {!diff}. *)
let rec diff (e : exp) : exp = 
  match e with
  | Const _ -> Const 0.0
  | Var -> Const 1.0
  | Plus (e1, e2) -> Plus (diff e1, diff e2)
  | Times (e1, e2) -> Plus (Times (diff e1, e2), Times (e1, diff e2))
  | Div (e1, e2) -> 
      Div (
        Plus (
          Times (diff e1, e2), 
          Times (Const (-1.0), Times (e1, diff e2))
        ), 
        Times (e2, e2)
      )
