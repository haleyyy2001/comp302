(* --- utils --- *)
(* --- don't worry too much about this part --- *)
let printl l = 
  List.iter (Printf.printf "%d ") l; Printf.printf "\n"; 
;;

let reverse_string s = 
  let rec helper i =
    if i >= String.length s then "" else (helper (i+1))^(String.make 1 s.[i]) 
  in 
  helper 0
;;

let prints s = 
  Printf.printf "%s\n" s
;;

exception Backtrack    
;;
(* --- print all subsets of a list --- *) 
(* --- 
subset [1; 2] should print 
2 1 
1 
2 
--- *)

let subset l =
  let rec helper l acc =
    match l with
    | [] -> printl acc; raise Backtrack
    | x :: xs -> try helper xs (x :: acc) with 
      | Backtrack -> helper xs acc
  in helper l []
;;
let l = [1; 2; 3]
in
subset l;;

(* --- generate correct parentheses sets of n pairs --- *) 
(* ---
parenthesis 2 should print 
(())
()()
--- *)

(* --- 
hint : use the open_count variable to keep track of how many parenthesis 
you have opened already, and continue accordingly 
--- *)

let parenthesis (n : int) =
  let rec helper (n : int) (acc : string) (open_count : int) =
    match n with
    | 0 -> prints (reverse_string acc); raise Backtrack 
    | x when open_count = 0 -> helper x ("(" ^ acc) (open_count + 1)
    | x when open_count = n -> helper (x - 1) (")" ^ acc) (open_count - 1)
    | x -> try helper x ("(" ^ acc) (open_count + 1) with 
      | Backtrack -> helper (x - 1) (")" ^ acc) (open_count - 1)
  in helper n "" 0
;;

parenthesis 3;;


(* --- determine if elems of list can sum up to target, 
and if yes, return them in a list ---*)

(* ---
sum 4 [1; 2; 3] should return [1; 3]
--- *)

let sum x l =
  let rec helper x l acc =
    match l with
    | _ when x = 0 -> acc
    | [] -> raise Backtrack
    | y :: ys -> try helper (x - y) ys (y :: acc) with
      | Backtrack -> helper x ys acc
  in helper x l []
;;

let l = [1; 2; 3;]
in
sum 4 l;;
