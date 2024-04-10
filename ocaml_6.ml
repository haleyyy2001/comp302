(* SECTION 1 *)

(*  Question 1.1 *)
let rec repeat (x : 'a) : 'a stream = { head = x; tail = mk_susp (fun () -> (repeat x)) }
  
(* Question 1.2 *)

let rec filter (f : 'a -> bool) (s : 'a stream) : 'a stream =
  if f s.head then 
    { head = s.head; tail = mk_susp (fun () -> filter f (force s.tail)) }
  else
    filter f (force s.tail)



 

(* Question 1.3 *) 
let rec lucas1 = {
  head = 2;
  tail = Susp (fun () -> lucas2);
}
and lucas2 = {
  head = 1;
  tail = Susp (fun () -> zip_with (+) lucas1 lucas2);
}


(* Question 1.4 *)
let rec unfold (f : 'a -> 'b * 'a) (seed : 'a) : 'b stream =
  let (h, new_seed) = f seed in
  { head = h; tail = mk_susp (fun () -> unfold f new_seed) }


(* Question 1.5 *)
let lucas_seed_func (x, y) = (x, (y, x + y))

let unfold_lucas = unfold lucas_seed_func (2, 1)

(* SECTION 2 *)
(* Question 2.1 *)
let rec scale (s1 : int stream) (n : int) : int stream =
  {
    head = s1.head * n;
    tail = mk_susp (fun () -> scale (force s1.tail) n)
  }

let rec merge (s1 : 'a stream) (s2 : 'a stream) : 'a stream =
  if s1.head = s2.head then {
    head = s1.head;
    tail = Susp (fun () -> merge (force s1.tail) (force s2.tail))
  }
  else if s1.head < s2.head then {
    head = s1.head;
    tail = Susp (fun () -> merge (force s1.tail) s2)
  }
  else {
    head = s2.head;
    tail = Susp (fun () -> merge s1 (force s2.tail))
  }

 

(* Question 2.2 *)
 
let rec s = 
  { head = 1;
    tail = Susp (fun () -> 
        merge 
          (merge (scale s 2) (scale s 3))
          (scale s 5)
      )
  }
