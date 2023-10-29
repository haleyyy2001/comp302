(* Here's the definition of 'append' we worked with last week: *)
let rec append l1 l2 =
  match l1 with
  | [] -> l2
  | x :: xs -> x :: append xs l2

(* We mentioned that it's not so easy to make this tail-recursive.
   Today we will do it, using continuations! *)
let rec append_tr l1 l2 return =
  match l1 with |x:: xs ->append_tr xs l2 (fun r -> x::r)
                |[] -> l2
(* Then we'll talk about theory and intuition for a while... *)
(* The posted code will have more exercises. *)

(* And then we'll look at how this helps us write a good
   tree-search algorithm. *)
(* For that, we'll need a type of (binary) trees: *)
type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

let rec find (p : 'a -> bool) (t : 'a tree) : 'a option =
  match t with |Empty -> None
               |Node(l,x,r) -> if p x then Some x
                   else match find p l with |None -> find p r
                                            |Some x  -> Some x
                       
    
let rec find1 p t return =
  match t with |Empty -> return None
               |Node(l,x,r) -> if p x  then return (Some x) 
                   else find1 p l ( fun result -> match result with 
                       |None -> find1 p r return 
                       |Some x-> return (Some x ))

let rec find2 p t fail succeed =
  match t with |Empty -> fail()
               |Node(l,x,r) -> if p x  then succeed (x )
                   else find2 p l (fun () -> find2 p r fail succeed) succeed
