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
  (* Add functions: fn x => e. Also add applications: e1 e2. *)
  | Fn of name * exp
  | App of exp * exp
and dec =
  | Dec of name * exp

           
type value
  = VInt of int
  | VBool of bool
  | VFun of name * exp (* Functions are values! *)
              (* This leads to changes to the 'eval' function. *)
                     
  (* Need some way of representing sets of free variables...
   OCaml has a way to make proper sets, but it needs something we haven't
   seen yet. Rather than add more confusion to a tricky topic, we'll just
   use lists and duplicates won't be a problem.
*)

type free_vars = name list
let union (fvs1 : free_vars) (fvs2 : free_vars) = fvs1 @ fvs2
let delete (x : name) (fvs : free_vars) =
  let p y = y <> x in
  List.filter p fvs 
                    
(* Compute free variables, exactly as seen on Slide 12 *)
let rec fv (e : exp) : free_vars =
  match e with
  | Int _ | Bool _ -> []
  | Var x -> [x]
  | If (ec, et, ef)     -> union (fv ec) (union (fv et) (fv ef))
  | Op (e1, _, e2)      -> union (fv e1) (fv e2)
  | Let (Dec (x, e), b) -> union (fv e) (delete x (fv b)) 
  | Fn (x, e)           -> delete x (fv e) (* fn x => e binds x in e *)
  | App (e1, e2)        -> union (fv e1) (fv e2) (* recurse normally *)
                                                          
(* When we do renaming, we have to pick a "new" name - that is, one which
won't accidentally capture free variables of e1 or b. This function does
just that, by generating names until one is safe. *)
let new_name_for (old : name) (e1 : exp) (b : exp) =
  let names_to_avoid = union (fv e1) (fv b) in
  let safe n = not (List.mem n names_to_avoid) in
  let rec generate_safe_name n =
    let new_name = old ^ string_of_int n in
    if safe new_name then new_name 
    else generate_safe_name (n+1)
  in generate_safe_name 1

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
  | Let (Dec (y, e), b) when x = y -> 
      Let (Dec (y, subst e1 x e), b) (* Don't touch b - x wouldn't be free! *)
  | Let (Dec (y, e), b) when List.mem y (fv e1) ->
      let z = new_name_for y e1 b in (* rename y to avoid accidental capture *)
      let renamed_b = subst (Var z) y b in (* rename... *)
      Let (Dec (z, subst e1 x e), subst e1 x renamed_b) (* finish subst *)
  | Let (Dec (y, e), b) ->
      (* x would be free in b, and no risk of accidental capture...
         The plain recursive substitutions will do the right thing. *)
      Let (Dec (y, subst e1 x e), subst e1 x b)
  
   (* Just like Let, Fn is a binding construct. So all these dangerous
   edge cases that we saw with Let will apply to Fn as well. If we added any
   more binding constructs, we'd probably want to make a helper function.
   That's an exercise for you! *)
  | Fn (y, e) when x = y -> Fn (y, e) (* Don't touch b - x would not be free! *)
  | Fn (y, e) when List.mem y (fv e1) ->
      let z = new_name_for y e1 e in
      let renamed_e = subst (Var z) y e in
      Fn (z, subst e1 x renamed_e)
  | Fn (y, e) -> Fn (y, subst e1 x e)
  
  (* For applications, we can recurse normally. *)
  | App (e2, e3) -> App (subst e1 x e2, subst e1 x e3)
      
                             
let exp_of_value (v : value) : exp =
  match v with
  | VInt n -> Int n
  | VBool b -> Bool b 
  | VFun (x, e) -> Fn (x, e)
                      
(* let x = 1 in x + x end *)
(* Evaluating this now correctly gives VInt 2. Try it! *)
let let_ex = Let (Dec ("x", Int 1), Op (Var "x", Plus, Var "x"))

(* [y/x](let y = 2 in x + y end *) 
(* Check that we now get the right answer! *)
let disaster_ex = subst (Var "y") "x"
    (Let (Dec ("y", Int 2), Op (Var "x", Plus, Var "y")))
      
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
      | VFun _ -> raise (RuntimeException "function in condition of if")
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
  
   (* Implement the rules for functions - recall that functions are values,
   so the rule B-Val applies to them. *)
  | Fn (x, e) -> VFun (x, e)
  | App (e1, e2) ->
      let v1 = eval e1 in
      let v2 = eval e2 in
      begin match v1 with
        | VFun (x, e) -> eval (subst (exp_of_value v2) x e)
        | _ -> raise (RuntimeException "Can't apply non-function!")
      end

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