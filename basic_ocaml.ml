(* Use this area to take notes and otherwise mess around with LearnOCaml outside of an assignment. *)
type 'a susp=Susp of (unit -> 'a)
let make_susp (f:unit ->'a) : 'a susp = Susp f
let force(Susp f : 'a susp) : 'a= f ()
type 'a stream=
  |Next of 'a * ('a stream) susp