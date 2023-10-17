(* "Susp"ended computations - will EVENTUALLY produce something
    of type 'a, but not until we ask it to produce a value. *)
type 'a susp = Susp of (unit -> 'a)

(* Create a new suspension. That is, wrap up a `fun () -> [suspended expression]`
    in a Susp constructor. *)
let mk_susp (f : unit -> 'a) : 'a susp = Susp f

(* _Force_ a suspension. This means "demand the delayed value." Forcing
    a suspension is the main "unit of work" performed during lazy evaluation. *)
let force (Susp f : 'a susp) : 'a = f ()

(*
OK, so let's now use our new trick of delaying values.
In particular, let's use it to represent _infinite streams_.

Recall the definition of the list type:
type 'a list =
  | []
  | (::) of 'a * 'a list

Since OCaml is an _eager_ language, if someone gives you a 'a list, it
must be finite. An infinite list would've taken infinite time to evaluate,
so it can't exist. In other words, a definition like this:

let rec nat_list_from (n : int) = n :: nat_list_from (n + 1)

is not useful. I can try and get the infinite list of all natural numbers
by calling nat_list_from 0. But this call needs the result of nat_list_from 1
before it can return. That needs the result of nat_list_from 2, etc. So n will
keep getting bigger and bigger, forever, and nat_list_from will never return.

Because all lists have to be finite, the list type has the [] constructor.
AKA the "nil" constructor. This constructor _ends_ a list, making it finite.

The problem with our list that never ends is that we are _eagerly evaluating_
the rest of the list. We are trying to put _infinite data_ in our data.

Instead, we can create data which is not fully evaluated. We can "suspend" bits
of it, using our 'a susp type. In particular, we'll suspend the _infinite_ bit.

The result is a type of "neverending list." We call these _streams_. So we get
rid of the [] case, and suspend the "rest of the stream" in the Cons case.
*)  
type 'a stream =
  | Next of 'a * ('a stream) susp

(*
But if our streams aren't fully there, how can we inspect the way they were built?
The answer is that we can't. Instead of looking at how they were built, all we can do
is _observe_ what they are.

Whenever we work with infinite data, the key question is "what can we observe from this?"
From a stream, we can observe the first element...
*)

let hd (str : 'a stream): 'a =
  match str with 
  | Next (x, _) ->x


(*
Or we can observe "the rest of the stream." In order to do this, we have to extract
the suspended tail, and then _force_ it, to actually produce the next cell.
                                                                        *)  


let tl (str : 'a stream) : 'a stream =
  match str with
  | Next (_, susp) -> force susp

(*
Now we want to be able to explore what elements are actually in a stream.
To see many elements at once, we'll pull the first n elements out of the stream
and return them as a (finite) OCaml list.
*)
let rec take (n : int) (str : 'a stream) : 'a list =
  (* If we want to take 0 (or fewer) elements from the stream... *)
  if n <= 0
  then [] (* Then we're already done! Produce the empty list. *)
  (* Otherwise...
     We want the first element of the stream at the start of our list.
     Then we want to follow that by the list that we get from pulling
     (n-1) elements off the _rest_ of the stream.
     To get the _rest_ of the stream, we need to use our 'tl' observation. *)
  else hd str :: take (n-1) (tl str)

(* Let's define some streams!
    Here's the infinite stream of 1. It's an infinite stream,
    every element of which is 1. *)
let rec ones () = Next (1, mk_susp ones) 
    
    
let get_ones n =take n (ones())
    
    
let rec mk_nats ( n: int) =
  Next( n, mk_susp (fun() -> mk_nats (n+1))) 
let nats = mk_nats 0
let rec zip_str_list ( xs: 'a stream) (ys : 'b list) : ('a*'b) list=
  match ys with 
  | [] -> []
  | (y::ys) -> ( hd xs, y) :: zip_str_list (tl xs) ys
                 
let enumerate (xs : 'a list) : (int * 'a) list = zip_str_list nats xs
    
 (* It doesn't matter how many elements are in 'xs' now. There could be
    1000000, or there might only be 10. Either way, 'nats' will supply
    enough indices to combine with the elements of 'xs'. We don't need
    to create a new range of indices that stops at the right point.
    
    As a result, we don't need to compute the _length_ of 'xs' either.
    Unfortunately, that's not helping us, because lists aren't lazy!
    Just to produce a result, we have to evaluate the entire result.
    
    What if we want to enumerate a list with 100000 elements, but after
    looking at 2 elements, we decide we're done? Then we shouldn't have
    to waste time enumerating the other 499998 elements. So instead of
    doing this zip on _eager_ lists, lets zip together _lazy_ streams...
*)   
let zip'(xs: 'a stream) (ys:'b stream) : ('a * 'b) stream=
  Next (
    (hd xs,hd ys),
    mk_susp (fun () -> zip' (tl xs) (tl ys)) 
  )
    
(* Once again, this is a bit unsatisfying. We're only able to combine
    streams with this function by combining them pairwise into tuples.
    What if we want to do something fancier, like _add_ them pairwise?
    For this, we want a generalization that is a higher-order function. *)    

let rec zip_with (f:'a->'b->'c) (xs: 'a stream) (ys:'b stream):'c stream =
  Next (f (hd xs) (hd ys) , mk_susp(fun()-> zip_with f (tl xs) (tl ys) ))
(* We promised it was a _generalization_. That means we must be able to
    (hopefully easily) recover the function 'zip' by using 'zip_with'.
    
    'zip_with' takes a function to use to combine the two elements together.
    'zip' combined them into a tuple, no questions asked. Well that means
    we should be able to recover 'zip' by passing a function that combines
    its arguments into a tuple.
    
    And indeed: *)
let zip xs ys= zip_with (fun a b-> (a,b)) xs ys 
    
let rec fibs()=Next( 0,mk_susp fibs1) and 
  fibs1() = Next(1, mk_susp ( fun() -> zip_with (+) (fibs()) (fibs1())))









    