
type name = string

type exp =
  | Int of int
  | Bool of bool
  | Var of name
  | If of exp * exp * exp
  | Let of name * exp * exp
  | Fun of name * tp * exp   (* fun (x:t) => e *)
  | App of exp * exp

and tp = TInt | TBool | TArrow of tp * tp
           
module Env = Map.Make(String) 
                              
(*

Contexts G ::= . | G, x:t

e !! v --- evaluation rule

G |- e : t  --- typing rule


--------------- T-Int  --------------- T-Bool
 G |- n : int           G |- b : bool

 G |- e1 : bool   G |- e2 : t   G |- e3 : t
-------------------------------------------- T-If
 G |- if e1 then e2 else e3 : t

 x : t in G
------------ T-Var
 G |- x : t
 
 
 G |- e1 : t'   G,x:t' |- e2 : t
---------------------------------- T-Let
    G |- let x = e1 in e2 : t
    
         G, x:t1 |- e : t
--------------------------------- T-Fun
    G |- fun x => e : t1 -> t
    
 G |- e1 : t' -> t     G |- e2 : t'
------------------------------------ T-App
         G |- e1 e2 : t
    
*)

type ctx = tp Env.t
    
exception TypeError of string

let rec infer (ctx : ctx) (e : exp) : tp = match e with
  | Int _ -> TInt
  | Bool _ -> TBool 
  | Var x -> begin match Env.find_opt x ctx with
      | None -> raise (TypeError "Unbound variable!")
      | Some t -> t
    end 
  | If (e1, e2, e3) ->
      let t1 = infer ctx e1 in
      begin match t1 with
        | TBool ->
            let t  = infer ctx e2 in
            let t' = infer ctx e3 in
            if t = t' then t
            else raise (TypeError "both arms of if must have the same type!")
        | _ -> raise (TypeError "condition of if must be a boolean!")
      end
  | Let (x, e1, e2) ->
      let t'   = infer ctx e1 in
      let ctx' = Env.add x t' ctx in
      let t    = infer ctx' e2 in
      t
  | Fun (x, t1, e) ->
      let t = infer (Env.add x t1 ctx) e in
      TArrow (t1, t)
        
  | App (e1, e2) -> begin match infer ctx e1 with
      | TArrow (t1, t) ->
          let t' = infer ctx e2 in
          if t1 = t' then t
          else raise (TypeError "Application of function to wrong type of argument!")
      | _ -> raise (TypeError "Can't apply non-fun!")
    end 

type value
  = VInt of int
  | VBool of bool
  | VFun of name * exp * env
and env = value Env.t 

      
exception RuntimeException of string 
      
        (*
  e !! true    e1 !! v          e !! false    e2 !! v
--------------------------     --------------------------
if e then e1 else e2 !! v      if e then e1 else e2 !! v
*)

let rec eval (env : env) (e : exp) : value = match e with
  | Int n -> VInt n
  | Bool b -> VBool b
  | If (cond, t, f) -> begin match eval env cond with
      | VBool true  -> eval env t
      | VBool false -> eval env f
      | _ -> raise (RuntimeException "Condition of if must be a boolean")
    end 
  | Var x -> begin match Env.find_opt x env with
      | None -> raise (RuntimeException ("Unbound variable: " ^ x))
      | Some v -> v
    end
  | Let (x, e, b) ->
      let v = eval env e in
      let env' = Env.add x v env in
      eval env' b
  | Fun (x, _, e) -> VFun (x, e, env)
  (* e1 e2 *)
  | App (e1, e2) ->
      begin match eval env e1 with
        | VFun (x, e, captured_env) ->
            let v    = eval env e2 in
            let env' = Env.add x v captured_env in
            eval env' e
        | _ -> raise (RuntimeException "Can't apply non-fun!")
      end
      
let do_eval e = eval Env.empty e

let oh_no = Let ("x", Int 1,
                 Let ("x", Int 2, Var "x"))
    
let _ =
  (let x = 1 in fun y -> x) 0

let bad_ex = App (
    Let ("x", Int 1, Fun ("y", TInt, Var "x")),
    Int 0
  )

            (* 
 if 0 < 3 then 4 else 5
 *)
 
let ex = If (
    (Int 0), 
    Int 4, 
    Int 5
  )

let parse (input : string) : exp option = failwith "Exercise"