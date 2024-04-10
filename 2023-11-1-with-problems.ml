(* How do we encode expressions? *)

type op = Plus | Minus | Times | Lt | Eq

type name = string          

type exp
  = Int of int
  | Bool of bool
  | If of exp (* condition *) * exp (* then-branch *) * exp (* else-branch *)
  | Op of exp * op * exp
  | Var of name
  | Let of dec * exp
and dec =
  | Dec of name * exp

           
type value
  = VInt of int
  | VBool of bool
           
             (* subst e1 x e2 === [e1 / x] e2 *)
let rec subst (e1 : exp) (x : name) (e2 : exp) =
  match e2 with
  | Int n -> Int n
  | Bool b -> Bool b
  | Var y when x = y -> e1
  | Var y            -> Var y
  | Op (e2, op, e3) -> Op (subst e1 x e2, op, subst e1 x e3)
  | If (ec, et, ef) -> If (
      subst e1 x ec,
      subst e1 x et,
      subst e1 x ef
    )
  | Let (Dec (y, e), b) -> Let (Dec (y, subst e1 x e), subst e1 x b)
                             
let exp_of_value (v : value) : exp =
  match v with
  | VInt n -> Int n
  | VBool b -> Bool b
                             
                             (*
let x = 1 in x + x end *)
let let_ex = Let (Dec ("x", Int 1), Op (Var "x", Plus, Var "x"))

      
exception RuntimeException of string      

let do_op (op : op) (v1 : value) (v2 : value) = match op, v1, v2 with
  | Plus, VInt x, VInt y -> VInt (x + y)
  | Plus, VBool x, VBool y -> VBool (x || y)
  | Minus, VInt x, VInt y -> VInt (x - y)
  | Times, VInt x, VInt y -> VInt (x * y)
  | Times, VBool x, VBool y -> VBool (x && y)
  | Lt, VInt x, VInt y   -> VBool (x < y)
  | Eq, VInt x, VInt y   -> VBool (x = y)
  | _ -> raise (RuntimeException "Don't do that!")
      
        (*
  e !! true    e1 !! v          e !! false    e2 !! v
--------------------------     --------------------------
if e then e1 else e2 !! v      if e then e1 else e2 !! v
*)

let rec eval (e : exp) : value = match e with
  | Int n -> VInt n
  | Bool b -> VBool b
  | If (cond, t, f) -> begin match eval cond with
      | VBool true  -> eval t
      | VBool false -> eval f
      | VInt x -> if x <> 0 then eval t else eval f
    end
  | Op (e1, op, e2) ->
      let v1 = eval e1 in
      let v2 = eval e2 in
      do_op op v1 v2
  | Var x -> raise (RuntimeException ("Unbound variable: " ^ x))
  | Let (Dec (x, e), b) ->
      let v1 = eval e in
      let b' = subst (exp_of_value v1) x b in
      eval b'

let oh_no = Let (Dec ("x", Int 1),
                 Let (Dec ("x", Int 2),
                      Var "x"))
            (* 
 if 0 < 3 then 4 else 5
 *)
 
let ex = If (
    (Int 0), 
    Int 4, 
    Int 5
  )

let parse (input : string) : exp option = failwith "Exercise"