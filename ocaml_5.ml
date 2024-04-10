(* Question 1 : partition an int list into two lists of equal sum *)
(* TODO: test cases *)
let partition_option_tests : (int list * (int list * int list) option) list = [
  ([], Some ([], []));                             
  ([1; 2; 3], Some (([1;2], [3])));                                              
  ([1; 2; 4], None);                                           
  ([3; 1; 4; 2; 2], Some ([3; 1;2], [4;2]));                  
  ([1; 1; 1; 1; 2; 4], Some ([1; 1; 1;  2], [1;4]));     
                                 
    (* Add more tests as required *)
]

(* TODO: implement partition *)

let rec subsets lst =
  match lst with
  | [] -> [[]]
  | x::xs ->
      let sub_xs = subsets xs in
      sub_xs @ (List.map (fun ss -> x::ss) sub_xs)

let partition (ns : int list) : (int list * int list) =
  let total_sum = sum ns in
  if total_sum mod 2 <> 0 then raise NoPartitionFound
  else
    let half_sum = total_sum / 2 in
    let possible_subsets = subsets ns in
    let rec find_partition sub_list =
      match sub_list with
      | [] -> raise NoPartitionFound
      | subset::rest -> 
          if sum subset = half_sum then
            let rec complement accu input left = 
              match input, left with
              | [], _ -> List.rev accu
              | _, [] -> List.rev_append accu input
              | x::xs, y::ys when x = y -> complement accu xs ys
              | x::xs, y::_ -> complement (x::accu) xs left
            in (subset, complement [] ns subset)
          else
            find_partition rest
    in
    find_partition possible_subsets


(* this function turns the output of partition into a type option *)
(* the tests you write will be tested against this function *)
(* this is meant to allow you to write success and failure tests *)  
let partition_option (ns : int list) : (int list * int list) option =
  try Some (partition ns) with
  | NoPartitionFound -> None


(* Question 2A: find a list of int from distinct tuples that add up to tot *)
(*TODO: test cases *)
let choice_sum_option_tests : ((int * ((int * int) list)) * (int list) option) list = [
  ((6, [(1,5);(2,4);(3,4)]), Some [1;2;3]);
  ((5, [(1,5);(2,4);(3,4)]), None);
  ((10, [(1,5);(2,4);(3,4)]), Some [5;2;3]);
  ((15, [(5,10);(0,5);(5,10)]), Some [5;0;10])
]


(* TODO: implement subset_sum_Q2A *)
let rec choice_sum (n : int) (tuples : (int * int) list) : int list =
  match tuples with
  | [] -> if n = 0 then [] else raise NoSumFound
  | (a,b)::rest ->
      try
        a :: (choice_sum (n - a) rest)
      with NoSumFound -> 
        b :: (choice_sum (n - b) rest)

(* this function turns the output of choice_sum into a type option *)
(* the tests you write will be tested against this function *)
(* this is meant to allow you to write success and failure tests *)
let choice_sum_option (n : int) (tuples : (int * int) list) : int list option =
  try Some (choice_sum n tuples) with
  | NoSumFound -> None

(* Question 2B: find a subset of tuple that add up to a given tuple *)
(* TODO: test cases *)
let subset_sum_option_tests : (((int * int) * ((int * int) list)) * ((int * int) list option)) list = [
  (((5,10), [(1,5);(2,4);(3,1)]), None);
  (((6,10), [(1,5);(2,4);(3,1)]), Some [(1,5);(2,4);(3,1)]);
  (((10,15), [(5,10);(0,5);(5,10);(5,0)]), Some [(5,10);(0,5);(5,0)]);
  (((3,9), [(1,2);(1,2);(1,9)]), None)
]

(* TODO: implement subset_sum_Q2B_helper *)

let rec subset_sum (target: (int * int)) (tuples : (int * int) list) : (int * int) list =
  let (target_a, target_b) = target in
  match tuples with
  | [] -> if target_a = 0 && target_b = 0 then [] else raise NoSubsetFound
  | tuple::rest ->
      let (a,b) = tuple in
      try
        tuple :: (subset_sum (target_a - a, target_b - b) rest)
      with NoSubsetFound -> 
        subset_sum target rest

(* this function turns the output of subset_sum into a type option *)
(* the tests you write will be tested against this function *)        
(* this is meant to allow you to write success and failure tests *)
let subset_sum_option (target : int * int) (tuples : (int * int) list) : (int * int) list option =
  try Some (subset_sum target tuples) with
  | NoSubsetFound -> None
