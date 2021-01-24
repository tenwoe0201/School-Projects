(* Constructing lazy values in OCaml *)

(* Lazy datatypes and functions *)
type 'a lazee = 'a hidden ref

 and 'a hidden = Value of 'a 
               | Thunk of (unit -> 'a)

let delay (unit_to_x: unit -> 'a) : 'a lazee = 
  ref (Thunk unit_to_x)

let force (l: 'a lazee) : unit = match !l with
  | Value _ -> ()
  | Thunk f -> l := Value (f ())

let demand (l: 'a lazee) : 'a = 
  force l; 
  match !l with
  | Value v -> v
  | Thunk f -> raise (Failure "this should not happen")

(* Streams, using lazy values *)
type 'a stream = Cons of 'a * 'a stream lazee


(* Some examples streams from files developed in class. *)
let rec from n =
  Cons ( n, delay ( fun () -> from (n+1) ) )

let ones =
  let rec mk_ones () = Cons (1, delay ( mk_ones ) )
  in mk_ones ()

let nats = from 1


(* Some helpful functions from files developed in class. *)
let head (s: 'a stream) : 'a = match s with
  | Cons (v, _) -> v

let tail (s :'a stream) : 'a stream = match s with
  | Cons (_, tl) -> demand tl

let rec take (n: int) (s: 'a stream) : 'a list =
  match n with
  | 0 -> []
  | _ -> (match s with
          | Cons (h, t) -> h :: take (n-1) (demand t) 
         )

let rec filter (p: 'a -> bool) (s: 'a stream) : 'a stream =
  match s with
  | Cons (hd, tl) -> 
     let rest = delay (fun () -> filter p (demand tl)) in
     if p hd 
     then Cons (hd, rest)
     else demand rest

let rec map (f: 'a -> 'b) (s: 'a stream) : 'b stream =
  match s with
  | Cons (h, t) -> Cons (f h, delay (fun () -> map f (demand t)))

let rec zip (f: 'a -> 'b -> 'c) (s1: 'a stream) (s2: 'b stream) : 'c stream =
  match s1, s2 with
  | Cons (h1, t1), Cons (h2, t2) ->
     Cons (f h1 h2, delay (fun () -> zip f (demand t1) (demand t2)))


(* Below is a stream of factorials.  It uses, the same definition of
   factorials as we developed in class except that the built in
   multiplication operator is replaced by a function `mul_p` that
   still multiplies its arguments but prints out those arguments as
   well.  *)

let mul_p x y =
  let () = print_endline ("multiplying " ^ string_of_int x ^ " and " ^
                            string_of_int y ^ ".")
  in x * y

let rec factorials () =
  Cons (1, delay (fun () -> zip mul_p nats (factorials ())))

let facts = factorials ()

let () =
  assert (take 5 facts = [1; 1; 2; 6; 24])
(* Please write your solutions below. *)
let rec cubes_from n = 
  Cons ((n*n*n), delay (fun () -> cubes_from (n+1)))
let cubes_from_map n = 
  map (fun a -> a*a*a) (from n)
let cubes_from_zip n = 
  let stream1 = zip ( * ) (from n) (from n) (* same as n^2*)
  in 
  zip ( * ) (from n) (stream1) (* doing n^2 * n = n^3*)
let facts' = 
  let ref_1 = ref nats
  in 
  let facts_1 = Cons (1, delay (fun () -> zip mul_p nats (! ref_1)))
  in
  let () = ref_1 := facts_1
  in
  facts_1
(*facts' uses references to keep track of the previous factorial so it doesn't have to find the factorial of (n-1) again*)
let sift (a : int) (stream_1 : int stream) : int stream =
  filter (fun b -> b mod a <> 0) stream_1
let rec sieve (stream_1 : int stream) : int stream =
  match stream_1 with
  | Cons (h, t) -> Cons (h, delay (fun () -> sieve (sift h (demand t))))

