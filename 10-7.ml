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
let hd (str : 'a stream) : 'a =
  match str with
  | Next (x, _) -> x

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

(* Suppose we need a list containing 'n' many copies of '1'. We can get it
    for free now, by simply combining bits that we already have.
    We have an _infinite_ stream of '1's, and we have a way to truncate
    that stream to length 'n'. We can just combine those!

    If we weren't working in a lazy environment, we'd have to define a
    new function from scratch to do this. Laziness is letting us be,
    well, lazy! *)
let get_ones n = take n (ones ())

(* What about a more interesting stream?

This stream contains every natural number. No matter how many elements
you take, you'll always be able to get the next natural number after that
as well. *)

(* First: mk_nats : int -> int stream defines the stream of all natural numbers
    beginning with the given one. *)
let rec mk_nats (n : int) =
  (* We're defining a stream, so we better start with 'Next'! *)
  Next (
    n, (* We promised that we begin with the given number... *)
    mk_susp (* But the rest needs to be a suspension. What's the rest of the stream?... *)
      (fun () -> (* First, delay it, since it's infinite... *)
        mk_nats (n+1)) (* And finally; it's the stream of all natural numbers,
                          but starting one higher. *)
  )
(* Now the stream of all nats starts at 0. *)
let nats = mk_nats 0

(* Notice that even though `mk_nats` calls itself, it does so only "underneath"
    a suspension. As a result, it returns _immediately_. No infinite loop,
    because the recursive calls don't actually happen until someone tries to
    observe the 'tl'! *)

(* ----------------------- using streams ------------------------- *)

(*
Suppose we have a list of values, 'a list.

Now, for each element of the list, we want to perform some operation that
also depends on the index.

In Python, we might write this:
for i in range(len(xs)):
  ....
  use xs[i] and i
  ....

But this has downsides. Primarily, we have to compute the length of xs to do this,
which is risky. What if xs is actually infinite? Or what if we break after only
a couple of elements? Then the work we did to compute the length is wasted.

Instead, we want a way to cleverly do this using `map` - which already applies a
function to each element of a list. But `map` doesn't know about indices.
So if we have a 'a list, we want a way to convert it to (int * 'a) list, attaching
an index to each element.

We could write a function to do this from scratch...

let enumerate1 (xs : 'a list) : (int * 'a) list =
  let rec go n xs = match xs with
    | [] -> []
    | (x::xs) -> (n,x) :: go (n+1) xs
  in go 0 xs

But we're functional programmers! We'd much rather combine smaller,
re-usable parts together, instead of figuring out how to write this
definition from scratch.

We already have an infinite stream of natural numbers. Those match the
possible indices of a list of any length! What we really want is a function
that can walk over a list and a stream, and mesh them together like a zipper.

 list:   [x; y; z; ...] --\
                           >-- [(0,x); (1,y); (2,z); ...]
 stream: [0; 1; 2; .... --/

Eventually, the list will run out, and we won't use any more elements
of the stream.

Let's try that:
*)
let rec zip_str_list (xs : 'a stream) (ys : 'b list) : ('a * 'b) list =
  match ys with
  | [] -> []
  | (y::ys) -> (hd xs, y) :: zip_str_list (tl xs) ys

(* Now we can implement our 'enumerate' function very simply! *)
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

let rec zip' (xs : 'a stream) (ys : 'b stream) : ('a * 'b) stream =
  Next (            (* We're returning a stream, so once again, start with Next. *)
    (hd xs, hd ys), (* The first element is the first elements of xs and ys... *)
    mk_susp (fun () -> (* But the rest is a suspension... *)
      zip' (tl xs) (tl ys)) (* Which will zip up the rest of the streams, on demand. *)
  )

(* Once again, this is a bit unsatisfying. We're only able to combine
    streams with this function by combining them pairwise into tuples.
    What if we want to do something fancier, like _add_ them pairwise?

    For this, we want a generalization that is a higher-order function. *)

let rec zip_with (f : 'a -> 'b -> 'c) (xs : 'a stream) (ys : 'b stream)
  : 'c stream =
  Next (f (hd xs) (hd ys), mk_susp (fun () -> zip_with f (tl xs) (tl ys)))

(* We promised it was a _generalization_. That means we must be able to
    (hopefully easily) recover the function 'zip' by using 'zip_with'.

    'zip_with' takes a function to use to combine the two elements together.
    'zip' combined them into a tuple, no questions asked. Well that means
    we should be able to recover 'zip' by passing a function that combines
    its arguments into a tuple.

    And indeed: *)

let zip xs ys = zip_with (fun x y -> (x, y)) xs ys

(* So using that 'zip' function, we can implement 'enumerate' again.
But the original motivation was that we wanted to make 'enumerate'
out of re-usable parts. So let's see how else we might use zip_with. *)

(* --------------- Fancy example: fibonacci ------------------ *)

(*
Suppose we had the infinite stream of fibonacci numbers. How might it
be defined?

It better start with 0 and 1. But what about after that?

Well the third element of 'fibs' is the first element, plus the second.
The fourth element of 'fibs' is the second element, plus the third.
The fifth element of 'fibs' is the third element, plus the fourth.

Consider that if we have the infinite stream of all fibonacci numbers,
then we also have the infinite stream of all fibonacci numbers _except the first_.
Call that stream 'fibs1'.

Then the first element of 'fibs' needs to be 0. The second element of 'fibs',
which is also the first element of 'fibs1', needs to be 1.

The third element of 'fibs' is now the first element of 'fibs',
plus the first element of 'fibs1'.
The fourth element of 'fibs' is the second element of 'fibs',
plus the second element of 'fibs1'. Visually:

    fibs    |   fibs1   |   fibs starting at element 3
    --------|-----------|-----------------------------
      0     |     1     |     1
      1     |     1     |     2
      1     |     2     |     3
      2     |     3     |     5
      3     |     5     |     8
      5     |     8     |    ...
      8     |    ...    |    ...
     ...    |    ...    |    ...

The third column is the sum of the other two! With a bit of
_mutual recursion_, we can express this, using zip_with!
*)

(* So 'fibs' is 0, followed by 'fibs1'... *)
let rec fibs  () = Next (0, mk_susp fibs1)

(* And 'fibs1' is 1, followed by... *)
and     fibs1 () = Next (1,
  (* As always, a suspension... *)
  mk_susp (fun () ->
    (* And when this suspension is forced, we start zipping
       'fibs ()' (the first column of the table) with
       'fibs1 ()' (the second column of the table)
       and the result is the third column; "the rest of the fibonacci numbers." *)
    zip_with (+) (fibs ()) (fibs1 ())))

(*
Try running 'take 10 (fibs ())' and you'll see fibonacci numbers!

Try _tracing_ 'take 4 (fibs ())' and you'll see how lazy evaluation
leads to _demand-driven_ evaluation. Things aren't evaluated until we need
them. Hence the name "call-by-need."
*)

let rec nth (n : int) (str : 'a stream) : 'a =
  match n, str with
  | _, Next (x, _) when n <= 0 -> x
  | n, Next (_, susp) -> nth (n-1) (force susp)


let susp_map (f : 'a -> 'b) (s : 'a susp) : 'b susp =
  (* We have to be a bit careful, because we're returning a suspension.
     We don't want to immediately force our given `'a susp` because maybe
     the `'b susp` that we return won't be used. In that case, we don't want
     to do any work forcing the `'a susp` which isn't used.
     What that means is, we don't want to start with `force s`:
     this implementation would be bad:
       let a = force s in mk_susp (fun () -> a)
     instead, we want this: *)
  let go () = f (force s) in
  mk_susp go
  (* or equivalently:
       mk_susp (fun () -> f (force s)) *)


(* Slow fibonacci:
   use this to play with susp_map. *)
let rec fib (n : int) =
  if n = 0 then 0
  else if n = 1 then 1
  else fib (n-2) + fib (n-1)

(* At this point, I encourage you to look back at the lecture recording
to see examples of playing with suspensions and susp_map. We looked
at a few examples to see when evaluation actually happens. *)

let rec str_map (f : 'a -> 'b) (str : 'a stream) : 'b stream =
  let Next (h, t) = str in
  Next (
    f h,
    (* t : 'a stream susp *)
    (* str_map : ('a -> 'b) -> 'a stream -> 'b stream *)
    (* str_map : ('a -> 'b) -> ('a stream -> 'b stream) is exactly the same *)
    (* In general, f : A -> B -> C -> D is the same as f : A -> (B -> (C -> D)) *)

    (* str_map f : 'a stream -> 'b stream *)
    (* susp_map (str_map f) : 'a stream susp -> 'b stream susp *)
    susp_map (str_map f) t
  )


(* Let's trace 'take 2 (str_map (fun x -> x + a) nats)' *)
(* We did this live in the lecture and you may benefit from the recording.

take 2 (str_map (fun x -> x + 1) nats)
=> take 2 (str_map (fun x -> x + 1) (mk_nats 0))   <-- start evaluating mk_nats 0
=> take 2 (str_map (fun x -> x + 1)
                   (Next (0, mk_susp (fun () -> mk_nats 1))))
=> take 2 (let Next (h,t) = (Next (0, Susp (fun () -> mk_nats 1))) in
           in Next ( (fun x -> x + 1) h,
                     susp_map (str_map (fun x -> x + 1)) t ))

    h |-> 0
    t |-> Susp (fun () -> mk_nats 1)

=> take 2 (Next ( (fun x -> x + 1) 0,
                  susp_map (str_map (fun x -> x + 1))
                           (Susp (fun () -> mk_nats 1)) ))

=> take 2 (Next (1, Susp (fun () ->
                      str_map (fun x -> x + 1)
                        (force (Susp (fun () -> mk_nats 1))))))

At this point in the lecture I accidentally started evaluating
mk_nats 1. We can't do that yet, because it's under a `fun () -> ...`!
I notice after a couple of minutes and backtrack.

=> hd ... :: take 1 (tl ...)

=> 1 :: take 1 (force (Susp (fun () -> ...)))
=> 1 :: take 1 (str_map (fun x -> x + 1)
                        (force (Susp (fun () -> mk_nats 1))))

=> 1 :: take 1 (str_map (fun x -> x + 1)
                        (mk_nats 1))

At this point we need to evaluate mk_nats 1. Looking back, we can
see we've already traced how to do mk_nats 0 and this will be
basically identical. So rather than redo all that work, I write
=>* instead of => to indicate that many steps happen here.

=>* 1 :: take 1 (Next (2, Susp (fun () -> str_map (fun x -> x + 1)
                                            (force (Susp (fun () -> mk_nats 2))))))

=> 1 :: hd ... :: take 0 (tl ...)
=> 1 :: 2 :: take 0 (str_map (fun x -> x + 1)
                             (force (Susp (fun () -> mk_nats 2))))

=> 1 :: 2 :: []
== [1; 2]
*)

At this point we took a vote and the class wanted to see some code in a
real lazy language. The following code works in a Haskell interpreter,
but OCaml will get very upset if you try and enter it. The prompt for
my haskell interpreter is λ. (a lambda). Also, Haskell swaps the meaning
of :: and : with respect to OCaml. Haskell uses :: for type signatures
and : for making lists. :{ tells the interpreter that I want to write
a multiple-line definition and :} closes that definition.
-- makes a line comment.

λ. :{
 | fib :: Int -> Int
 | fib 0 = 0
 | fib 1 = 1
 | fib n = fib (n-2) + fib (n-1)
 | :}
λ.
λ. fib100 = fib 100
-- the interpreter gives the next prompt instantly, even though we
-- saw in ocaml that fib 100 takes a long time to evaluate. Since
-- everything is lazy by default and the interpreter doesn't need
-- to show us the result yet, *no work happens at all.*
λ. fib100 -- please show me the value! Now this hangs.
^CInterrupted.
λ. [0..]
[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,
29,30,31,32,33,34,35,36,37,38,39,^CInterrupted.
λ. fibs = map fib [0..] -- once again, this is instant.
λ. take 30 fibs
[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,
28657,46368,75025,121393,196418,317811,514229]
-- the first several appeared instantly, but it became visibly slower to produce
-- the last few. Even so, Haskell was able to lazily produce some output long
-- before the work had finished.

*)





