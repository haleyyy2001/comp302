(* Hi everyone. All of these problems are generally "one-liners" and have slick solutions. They're quite cute to think
   about but are certainly confusing without the appropriate time and experience that you devote towards reasoning about
   this style. Good luck! :-)  *)

(* For example, if you wanted to use the encoding of five in your test cases, you could define: *)
let five : 'b church = fun s z -> s (s (s (s (s z))))
(* and use 'five' like a constant. You could also just use
 'fun z s -> s (s (s (s (s z))))' directly in the test cases too. *)

(* If you define a personal helper function like int_to_church, use it for your test cases, and see things break, you should
   suspect it and consider hard coding the input cases instead *)



(* Question 1a: Church numeral to integer *)
(* TODO: Test cases *)
let zero : 'b church = fun s z -> z
let one : 'b church = fun s z -> s z
let two : 'b church = fun s z -> s (s z)
let three : 'b church = fun s z -> s (s (s z))
let four : 'b church = fun s z -> s (s (s (s z)))
let six : 'b church = fun s z -> s (s (s (s (s (s z)))))
let to_int_tests : (int church * int) list = [
  (zero, 0);
  (one, 1);
  (two, 2);
  (three, 3);
  (four, 4);
]
;;

 


(* TODO: Implement
   Although the input n is of type int church, please do not be confused. This is due to typechecking reasons, and for
   your purposes, you could pretend n is of type 'b church just like in the other problems.
*)
 
let to_int (n : int church) : int = 
  n (fun x -> x + 1) 0


(* Question 1b: Determine if a church numeral is zero *)
(* TODO: Test cases *)
let is_zero_tests : ('b church * bool) list = [
  (zero, true);
  (one, false);
  (two, false);
  (three, false);
  (four, false);
]
;;

let is_zero (n : 'b church) : bool = 
  n (fun _ -> false) true
 

(* Question 1d: Determine if a church numeral is odd *)
(* TODO: Test cases *)
let is_odd_tests : ('b church * bool) list = [
  (zero, false);
  (one, true);
  (two, false);
  (three, true);
  (four, false);
]
;;

let is_odd (n : 'b church) : bool =
  n (fun b -> match b with
      | true -> false
      | false -> true) false



(* Question 1e: Add two church numerals *)
(* TODO: Test cases *)
let add_tests : ( ('b church * 'b church) * 'b church) list = [
  ((zero, zero), zero);      (* 0 + 0 = 0 *)
  ((one, zero), one);        (* 1 + 0 = 1 *)
  ((zero, one), one);        (* 0 + 1 = 1 *)
  ((one, one), two);         (* 1 + 1 = 2 *)
  ((two, two), four);        (* 2 + 2 = 4 *)
  ((one, three), four);      (* 1 + 3 = 4 *)
  ((three, one), four);      (* 3 + 1 = 4 *)
]
;;


let add (n1 : 'b church) (n2 : 'b church) : 'b church =
  fun s z -> n2 s (n1 s z)


(* Question 1f: Multiply two church numerals *)
(* TODO: Test cases *)
let mult_tests : ( ('b church * 'b church) * 'b church) list = [
  ((zero, zero), zero);      (* 0 * 0 = 0 *)
  ((one, zero), zero);       (* 1 * 0 = 0 *)
  ((zero, one), zero);       (* 0 * 1 = 0 *)
  ((one, one), one);         (* 1 * 1 = 1 *) 
  ((one, three), three);     (* 1 * 3 = 3 *)
  ((three, one), three);     (* 3 * 1 = 3 *)
  ((two, three), six);       (* 2 * 3 = 6 *)
  ((three, two), six);       (* 3 * 2 = 6 *)
]
;;

let mult (n1 : 'b church) (n2 : 'b church) : 'b church =
  fun s z -> n1 ( n2 s ) z



(* Question 2a: Write a function taking an int and a church and returning the int to the power of the church *)
(* TODO: Test cases *)
let zero : 'b church = fun s z -> z
let one : 'b church = fun s z -> s z
let two : 'b church = fun s z -> s (s z)

let int_pow_church_tests : ((int * 'b church) * int) list = [
  ((2, zero), 1);  
  ((2, one), 2);  
  ((2, two), 4);   
  ((0,one),0);
]
;;

let int_pow_church (x : int) (n : 'b church) : int =
  n (fun acc -> x * acc) 1



(* Question 2b: Write a function taking tuple of church and incrementing both values of the tuple of the value of the church *)
(* TODO: Test cases *)
let swap_add_tests : (('b church * 'b church) * ('b church * 'b church)) list = [
  ((zero , zero) , (zero , zero)) ;
  ((one, zero) , (zero , one)) ;
  ((one , two) , (two , three)) 
  
  
]
;;

let swap_add (t : ('b church * 'b church)) : ('b church * 'b church) =
  let(a,b) =t in (b, fun s z -> a s (b s z))

let three : 'b church = fun s z -> s (s (s z))
let five : 'b church = fun s z -> s (s (s (s (s z))))
(* Question 2c: Write a function computing the nth term of the Fibonacci suite *)
(* TODO: Test cases *)
let fibo_tests : ('a church * 'b church) list = [
  (zero, zero);    
  (one, one);      
  (two, one);       
  (three, two);    
]
;;


 
let fibo (n : 'a church) : 'b church =
  fst( n swap_add(zero,one) )