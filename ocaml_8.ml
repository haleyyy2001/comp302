(* SECTION 1: Laziness *)

(* Question 1a *)

let rec lazy_insert (v : 'a) (l : 'a lazy_list) : 'a lazy_list = 
  match l with
  | LNil -> LCons (v, mk_susp (fun () -> LNil))
  |LCons (x,xs) -> if compare v x <= 0 then
        LCons (v, mk_susp (fun () -> l))
      else
        LCons (x, mk_susp (fun () -> lazy_insert v (force xs)))


(* Question 1b *)

let lazy_ins_sort (l : 'a lazy_list) : 'a lazy_list =
  let rec go (l : 'a lazy_list) (acc : 'a lazy_list) : 'a lazy_list =
    match l with
    | LNil -> acc
    |LCons (x,xs) -> go (force xs) ( lazy_insert x acc)
  in go l LNil

(* SECTION 2 : Backtracking *)

(* Question 2a *)
    
let rec tree_sum (t : int tree) (n : int) : int list =
  match t with 
  |Empty -> raise NoTreeSum
  |Tree (l,x,r) -> if x = n then [x]
      else if x > n then  raise NoTreeSum
      else try x :: (tree_sum r (n-x) )
        with NoTreeSum ->
          x:: (tree_sum l (n-x) ) 

 
  

(* Section 3 : References *)

(* Question 3a *)
let ( *= ) (x : int ref) (n : int) : int = 
  x := ( (!x) *n); !x

(* Question 3b *)
let make_piggybank () : piggybank =
  let balance = ref 0 in
  let broken = ref false in 
  let get_balance()=
    if !broken then raise BrokenPiggybank
    else ! balance
  in
  let add_to_piggybank amount=
    if !broken then raise BrokenPiggybank
    else balance :=   ( ! balance  + amount) 
  in
  let break_piggybank () =
    if !broken then raise BrokenPiggybank
    else (
      let final_balance = !balance in
      balance := 0; (* Technically unnecessary since it's broken, but good practice *)
      broken := true;
      final_balance
    )
  in

  { get_balance; add_to_piggybank; break_piggybank }
           
    

