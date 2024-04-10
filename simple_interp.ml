(* How do we encode expressions? *)

type op = Plus | Minus | Times | Lt | Eq

type exp
  = Int of int
  | Bool of bool
  | If of exp (* condition *) * exp (* then-branch *) * exp (* else-branch *)
  | Op of exp * op * exp

type value
  = VInt of int
  | VBool of bool
      
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

            (* 
 if 0 < 3 then 4 else 5
 *)
 
let ex = If (
    (Int 0), 
    Int 4, 
    Int 5
  )

let parse (input : string) : exp option = failwith "Exercise"    
