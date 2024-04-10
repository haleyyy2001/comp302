(**To DO: Write a good set of tests for free_variables **)
(* Tests for free_variables *)
let free_variables_tests = List.map (fun (e, vs) -> (e, VarSet.of_list vs))
    [
      (* Test for Fn *)
      (Fn (("x", Int), Var "y"), ["y"]);  (* 'y' is free *)
      (Fn (("x", Int), Var "x"), []);     (* 'x' is bound, not free *)

      (* Test for Rec *)
      (Rec ("f", Arrow (Int, Int), Fn (("x", Int), Var "f")), []); (* 'f' is bound by Rec *)

      (* Test for Let *)
      (Let ("x", Var "y", Var "x"), ["y"]);  (* 'y' is free, 'x' is bound *)

      (* Test for LetPair *)
      (LetPair ("x", "y", Primop (Comma, [Var "a"; Var "b"]), Var "x"), ["a"; "b"]); (* 'a' and 'b' are free *) 
      (* Test for Apply *)
      (Apply (Var "f", Var "x"), ["f"; "x"]);  (* Both 'f' and 'x' are free *)
(* Nested Fn and Let with shadowing *)
      (Fn (("x", Int), Let ("y", Var "x", Fn (("x", Int), Var "y"))), []);
(* Let within Fn, variable shadowing *)
      (Fn (("x", Int), Let ("x", I 1, Var "x")), []); 
(* Rec within Apply *)
      (Apply (Rec ("f", Arrow (Int, Int), Fn (("x", Int), Var "f")), I 5), []); 
(* Unused variable in Let *)
      (Let ("x", I 1, B true), []); 
(* Complex nested case *)
      (Let ("x", Fn (("y", Int), Var "y"), Apply (Var "x", I 2)), []); 
      (* Redundant variable definition *)
      (Let ("x", I 1, Let ("x", I 2, Var "x")), []);

(* Nested LetPair within Fn *)
      (Fn (("x", Int), LetPair ("y", "z", Primop (Comma, [Var "x"; I 1]), Var "y")), []);

(* Rec inside Let *)
      (Let ("f", Rec ("g", Arrow (Int, Int), Fn (("x", Int), Var "g")), Var "f"), []);

(* Extremely simple expression *)
      (Var "x", ["x"]);

(* Complex nested structure *)
      (Let ("x", Fn (("y", Int), LetPair ("z", "w", Primop (Comma, [Var "y"; I 2]), Var "z")),
            Apply (Var "x", I 1)), []);

      (* Additional tests for potential edge cases *)
      (* ... *)
    ]


(* TODO: Implement the missing cases of free_variables. *)
(* TODO: Implement the missing cases of free_variables. *)
let rec free_variables : exp -> VarSet.t =
  let union = VarSet.union in
  let remove = VarSet.remove in
  function
  | Var y -> VarSet.singleton y
  | I _ | B _ -> VarSet.empty
  | If (e, e1, e2) -> union (free_variables e) (union (free_variables e1) (free_variables e2))
  | Primop (_, args) -> List.fold_left (fun acc exp -> union acc (free_variables exp)) VarSet.empty args
  | Fn ((x, _), e) -> remove x (free_variables e)
  | Rec (x, _, e) -> remove x (free_variables e) 
  | Let (x, e1, e2) -> union (free_variables e1) (remove x (free_variables e2))
  | LetPair (x, y, e1, e2) -> union (free_variables e1) (remove x (remove y (free_variables e2)))
  | Apply (ef, ex) -> union (free_variables ef) (free_variables ex)


(* TODO: Write a good set of tests for subst. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let subst_tests : (((exp * name) * exp) * exp) list = [ 
 
  (* Test case for Let *)
  (((I 1, "x"), Let ("y", I 2, Primop (Plus, [Var "y"; Var "x"]))), Let ("y", I 2, Primop (Plus, [Var "y"; I 1])));

  (* Test case for LetPair *)
  (((B true, "x"), LetPair ("x", "y", Primop (Comma, [Var "x"; Var "y"]), Var "x")), LetPair ("x", "y", Primop (Comma, [B true; Var "y"]), Var "x"));

  (* Test case for Rec *)
  (((B true, "x"), Rec ("x", Arrow (Bool, Bool), Fn (("y", Bool), Var "x"))), Rec ("x", Arrow (Bool, Bool), Fn (("y", Bool), Var "x")));

  (* Test case for Fn *)
  (((I 1, "x"), Fn (("x", Int), Var "x")), Fn (("x", Int), Var "x"));

  (* Test case for Apply *)
  (((I 1, "x"), Apply (Fn (("x", Int), Var "x"), Var "x")), Apply (Fn (("x", Int), Var "x"), I 1));
(* Test for Let with nested Fn *)
  (((I 1, "z"), Let ("x", Fn (("z", Int), Var "z"), Apply (Var "x", I 2))), Let ("x", Fn (("z", Int), Var "z"), Apply (Var "x", I 2)));
 
 
(* Test for Rec with nested Let *)
  (((I 1, "y"), Rec ("f", Arrow (Int, Int), Let ("y", I 2, Var "f"))), Rec ("f", Arrow (Int, Int), Let ("y", I 2, Var "f")));

(* Test for Fn with nested LetPair and shadowing *)
  (((I 1, "x"), Fn (("x", Int), LetPair ("x", "y", Primop (Comma, [Var "x"; Var "y"]), Var "x"))), Fn (("x", Int), LetPair ("x", "y", Primop (Comma, [Var "x"; Var "y"]), Var "x")));

(* Test for Apply with nested Rec *)
  (((I 1, "x"), Apply (Rec ("f", Arrow (Int, Int), Fn (("y", Int), Var "f")), Var "x")), Apply (Rec ("f", Arrow (Int, Int), Fn (("y", Int), Var "f")), I 1))




]

 

(* TODO: Implement the missing cases of subst. *)
(* TODO: Implement the missing cases of subst. *)

 (* TODO: Implement the missing cases of subst. *)
 (* TODO: Implement the missing cases of subst. *)
let rec subst s exp =
  let (e', x) = s in
  match exp with
  | Var y ->
      if x = y then e'
      else Var y
  | I n -> I n
  | B b -> B b
  | Primop (po, args) -> Primop (po, List.map (subst s) args)
  | If (e, e1, e2) -> If (subst s e, subst s e1, subst s e2)
  | Let (y, e1, e2) ->
      let e1' = subst s e1 in
      if y = x then
        Let (y, e1', e2)
      else
        Let (y, e1', subst s e2)
  | LetPair (y1, y2, e1, e2) ->
      let e1' = subst s e1 in
      if y1 = x || y2 = x then
        LetPair (y1, y2, e1', e2)
      else
        let (new_names, e2') = rename_all [y1; y2] e2 in
        let new_y1, new_y2 = match new_names with
          | n1 :: n2 :: _ -> (n1, n2)
          | _ -> failwith "rename_all did not return two names"
        in
        LetPair (new_y1, new_y2, e1', subst s e2')

  | Rec (y, t, e) ->
      if y = x then
        Rec (y, t, e)
      else
        Rec (y, t, subst s e)
  | Fn ((y, t), e) ->
      if y = x then
        Fn ((y, t), e)
      else
        Fn ((y, t), subst s e)
  | Apply (ef, ex) -> Apply (subst s ef, subst s ex)

and rename (x : name) (e : exp) : name * exp =
  let x' = fresh_var x in
  (x', subst (Var x', x) e)

and rename_all (names : name list) (exp : exp) : (name list * exp) =
  List.fold_right
    (fun name (names, exp) ->
       let (name', exp') = rename name exp in
       (name' :: names, exp'))
    names
    ([], exp)
 


(* Applying a list of substitutions to an expression, leftmost first *)
(* You don't need this for HW9, but it might help to understand what this
   function does and imagine why it might be useful. It MIGHT be helpful
   in HW10, depending on what we ask you to do. *)
let subst_list subs exp =
  List.fold_left (fun exp sub -> subst sub exp) exp subs
