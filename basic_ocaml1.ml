(* 
   Lets say bool <= int <= float

   find a subtype S <= T = int -> bool -> (bool -> int -> int)
   that does not match the given type in any position, unless necessary
   

  rule for ->
     t->t' < s -> s' if s < t and t'< s'
  
  
  T = int -> (bool -> (bool -> int -> int))
float > int



? < bool -> (bool -> int -> int)
   
   int > bool
   
   
? < bool -> (int -> int)
(int -> float -> bool)
  
  
  
  
  S = float -> int -> ( int -> float -> bool )


int < int -> float


T = ref int -> bool -> (bool -> int) -> (int, float)
S < T
ref int => ref int
float > bool 
? > (bool -> int)  => bool-> float
 bool < bool
float > int
? < (int, float)
(bool, int) < (int, float)

S = ref int -> float -> (bool-> float) -> (bool, int)

*)


type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree
              
  
let rec mirror t =
  match t with 
  | Empty -> Empty
  | Node(tl, v, tr) -> Node(mirror tr, v, mirror tl)
                         
                         
let rec inverse t =
  match t with
  | Empty-> Empty
  | Node(tl, v, tr) -> Node(inverse tl, -v, inverse tr)
                         
                         
(*
   prove inverse(mirror t) == mirror(inverse t)
   base case t = Empty :
   inverse(mirror t) == mirror(inverse t)
   inverse(Empty) == mirror(Empty)
   Empty == Empty

   t = Node (tl, v tr)
   IH :
   inverse(mirror tl) == mirror(inverse tl)
   inverse(mirror tr) == mirror(inverse tr)

 prove inverse(mirror Node (tl, v tr)) == mirror(inverse Node (tl, v tr))

   LHS 
        inverse(mirror Node(tl, v, tr)) = 
        inverse(Node(mirror  tr, v, mirror tl)) =
        Node(inverse mirror tr, -v, inverse mirror tl)


RHS    mirror(inverse Node (tl, v tr))
       mirror(Node(inverse tl, -v, inverse tr)
       Node( mirror inverse tr, -v,  mirror inverse tl)
Apply Induction Hyps
      Node(inverse mirror tr, -v, inverse mirror tl)
       


   



*)


