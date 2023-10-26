(* Reviewing some basic stream functions *)
let rec take n s =
  match n with
  | 0 -> []
  | _ -> s.head :: (take (n-1) (force s.tail))
    
let rec nth n s =
  match n with
  | 0 -> s.head
  | _ -> nth (n-1) (force s.tail)
    
let sum n s = 
  List.fold_left (fun x y -> x + y) 0 (take n s)
    
let sum' n s =
  List.fold_left (+.) 0. (take n s)
  
(* Defining some simple infinite streams *)

let ones = iterate (fun x -> x) 1

let nats = iterate (fun x -> x + 1) 0

let from x = iterate (fun x -> x + 1) x 

(* Taylor series expansions *) 
(* e is approximated by 1/0! + 1/1! + 1/2! + ... *)
let rec fact = function 0 -> 1 | n -> n * (fact (n-1)) ;;
let facts = str_map fact nats
let f_ones = str_map float_of_int ones
let f_facts = str_map float_of_int facts
    
let e = zip_with (fun x y -> x /. y) f_ones f_facts
  
(* pi is approximated by 4/1 - 4/3 + 4/5 - ... *) 
let alt_fours = iterate (fun x -> -.x) 4.
let odds = iterate (fun x -> x +. 2.) 1.

let pi = zip_with (/.) alt_fours odds
  
  

(* The sieve of Erastothenes *)

    (* [2;3;5;7;11;13] *)

let primes = 
  let nums = from 2 in
  let rec sieve x =
    let next = filter (fun n -> (n mod x.head) <> 0) (force x.tail) in
    { head = x.head ; tail = Susp (fun () -> sieve next) }
  in sieve nums 
