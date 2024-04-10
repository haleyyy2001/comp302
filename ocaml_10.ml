(* TODO: Write a good set of tests for eval. *)
(* TODO: Write a good set of tests for eval. *)
let eval_tests = [
  (* Example test case for Let *)
  (Let ("x", I 1, Primop (Plus, [Var "x"; I 5])), I 6);

  (* Test case for Rec: factorial function *)
(* Test case for Rec: Fibonacci function *)
  (Let ("fib",
        Rec ("fib",
             Fn ("n",
                 If (Primop (Equals, [Var "n"; I 0]),
                     I 0,
                     If (Primop (Equals, [Var "n"; I 1]),
                         I 1,
                         Primop (Plus, [Apply (Var "fib", Primop (Minus, [Var "n"; I 1]));
                                        Apply (Var "fib", Primop (Minus, [Var "n"; I 2]))]
                                ))))),
        Apply (Var "fib", I 5)),
   I 5);  (* Expected outcome is I 5, the 5th Fibonacci number *)


  (* Test case for Apply: applying a simple function *)
  (Apply (Fn ("x", Primop (Plus, [Var "x"; I 1])), I 2), I 3);
 
         
  (* Test case for LetPair: decomposing a pair and using its elements *)
  (LetPair ("x", "y", Primop (Comma, [I 2; I 3]),
            Primop (Plus, [Var "x"; Var "y"])), I 5);
]


(* TODO: Implement the missing cases of eval. *)
let rec eval exp =
  match exp with
  (* Values evaluate to themselves *)

  | Var x -> raise (Stuck (Free_variable x))

  (* Primitive operations: +, -, *, <, = *)
  | Primop (po, args) ->
      let args = List.map eval args in
      begin
        match eval_op po args with
        | None -> raise (Stuck Bad_primop_args)
        | Some v -> v
      end

  | If (e, e1, e2) ->
      begin
        match eval e with
        | B true -> eval e1
        | B false -> eval e2
        | _ -> raise (Stuck If_non_true_false)
      end

  | Let (x, e1, e2) ->
      let e1 = eval e1 in
      eval (subst (e1, x) e2)
      
  | LetPair (x1, x2, e1, e2) ->
      begin
        match eval e1 with
        | Primop (Comma, [v1; v2]) ->
            eval (subst_list [(v1, x1); (v2, x2)] e2)
        | _ -> raise (Stuck LetPair_non_tuple)
      end
  | I _ -> exp
  | B _ -> exp
  | Fn _ -> exp
    
  | Rec (f, e) -> 
      Rec (f, e) 
 
  | Apply (ef, ex) ->
      begin
        match eval ef with
        | Fn (x, e') ->
            let v1 = eval ex in
            eval (subst (v1, x) e')
        | Rec (f, e') -> 
            (match e' with
             | Fn (x, e'') -> 
                 let v1 = eval ex in
                 let self = Rec (f, e') in
                 eval (subst (self, f) (subst (v1, x) e''))
             | _ -> raise (Stuck Apply_non_fn))  (* Handle non-function case *)
        | _ -> raise (Stuck Apply_non_fn)
      end


(* Part 2: unify *)
(* TODO: Implement missing cases. *)
let rec unify (cs : (tp * tp) list) : tp TVarMap.t =
  (* helper function *)
  let rec occurs (x : name) (tp : tp) : bool =
    match tp with
    | Int | Bool -> false
    | Arrow (a, b) -> occurs x a || occurs x b
    | Pair (a, b) -> occurs x a || occurs x b
    | TVar y -> x = y
  in
  match cs with
  | [] -> TVarMap.empty
  | (t1,t2) :: cs when t1 = t2 -> unify cs
  | (Arrow (t1, t2), Arrow (s1, s2)) :: cs ->
      unify ((t1, s1) :: (t2, s2) :: cs)

  | (Pair (t1, t2), Pair (s1, s2)) :: cs ->
      unify ((t1, s1) :: (t2, s2) :: cs)

  | (TVar a, t) :: cs | (t, TVar a) :: cs ->
      if occurs a t then
        raise (UnifError UnifOccursCheckFails)
      else
        let subst = TVarMap.singleton a t in
        let cs = List.map (fun (t1, t2) -> (apply_type_substitution subst t1, apply_type_substitution subst t2)) cs in
        let rest_unifier = unify cs in
        TVarMap.add a (apply_type_substitution rest_unifier t) rest_unifier

  | (TVar a, t) :: cs | (t, TVar a) :: cs ->
      if occurs a t then
        raise (UnifError UnifOccursCheckFails)
      else
        let subst = TVarMap.singleton a t in
        let cs = List.map (fun (t1, t2) -> (apply_type_substitution subst t1, apply_type_substitution subst t2)) cs in
        let rest_unifier = unify cs in
        TVarMap.add a (apply_type_substitution rest_unifier t) rest_unifier

  | (t1, t2) :: _ -> type_mismatch t1 t2

  
(* Part 3: infer *)
let ctx_of_list = extend_list Context.empty
let infer_tests =
  let f ((bs, e), ty) = ((ctx_of_list bs, e), ty) in
  List.map f [
    (* Existing test *)
    (([("x", Int)], Var "x"), Int);
    
    ((* Test for an integer literal *)
      ([], I 42), Int);

    ((* Test for a boolean literal *)
      ([], B true), Bool);

    ((* Test for a function application *)
      ([("f", Arrow (Int, Bool))], Apply (Var "f", I 5)), Bool);

    ((* Test for a recursive function *)
      ([], Rec ("fact", Fn ("n", If (Primop (Equals, [Var "n"; I 0]), I 1, Primop (Times, [Var "n"; Apply (Var "fact", Primop (Minus, [Var "n"; I 1]))]))))), Arrow (Int, Int));

  ]  
(* set up helper functions for infer *)
(* list of constraints *)
let constraints = ref [] ;;
(* helper function to add a constraint *)
let constrain t1 t2 = constraints := (t1,t2) :: !constraints ;;
let ctr = ref 0 ;;
(* helper function that generates a new TVar with names like tv1, tv2, ... *)
let freshvar () =
  let n = !ctr in
  ctr := 1 + !ctr;
  TVar ("tv" ^ string_of_int n) ;;
 
exception InferError of string


(* TODO: Implement the missing cases for infer *)
let infer (gamma : context) (e : exp) : tp =
  (* reset constraint list, so that we don't see constraints from other calls. *)
  constraints := [];
  (* reset the counter. In a real compiler, we should NOT do this, but in this case
     it will make it so that you see the same type variable names in the toplevel
     and your grade report, which we imagine will be helpful :) *)
  ctr := 0;
  let rec infer_aux (gamma : context) (e : exp) : tp =
    match e with
    | I _ -> Int
    | B _ -> Bool
    | Var x -> begin match lookup x gamma with
        | Some t -> t
        | None -> free_variable x
      end
    | If (e1, e2, e3) ->
        let t1 = infer_aux gamma e1 in
        let t2 = infer_aux gamma e2 in
        let t3 = infer_aux gamma e3 in
        constrain t1 Bool; (* Ensure condition is Bool *)
        constrain t2 t3;   (* Then and Else branch should have the same type *)
        t2                 (* Return type of the branches *)
 

    | Primop (op, args) ->
        let (arg_types, return_type) = primopType op in
        List.iter2 (fun arg expected_type ->
            let arg_type = infer_aux gamma arg in
            constrain arg_type expected_type
          ) args arg_types;
        return_type

    | Fn (x, b) ->
        let x_type = freshvar () in
        let new_gamma = extend gamma (x, x_type) in
        let b_type = infer_aux new_gamma b in
        Arrow (x_type, b_type)

    

    | Rec (f, b) ->
        let f_type = freshvar () in
        let new_gamma = extend gamma (f, f_type) in
        let b_type = infer_aux new_gamma b in
        constrain f_type b_type;
        b_type


    | Let (x, e, b) ->
        let e_type = infer_aux gamma e in
        let new_gamma = extend gamma (x, e_type) in
        infer_aux new_gamma b

    | LetPair (x1, x2, e, b) ->
        let e_type = infer_aux gamma e in
        let x1_type = freshvar () in
        let x2_type = freshvar () in
        let new_gamma = extend (extend gamma (x1, x1_type)) (x2, x2_type) in
        constrain e_type (Pair (x1_type, x2_type));
        infer_aux new_gamma b

    | Apply (ef, ex) ->
        let ef_type = infer_aux gamma ef in
        let ex_type = infer_aux gamma ex in
        match ef_type with
        | Arrow (param_type, return_type) ->
            constrain ex_type param_type;
            return_type
        | _ -> raise (InferError "Attempted to apply a non-function type")


  in

  let inferred_type = infer_aux gamma e in
  let unify_solution = unify !constraints in
  apply_type_substitution unify_solution inferred_type