(* Code for implementing lazy streams in OCaml.  This is a shortcut that does not explicitly 
introduce delay and force.  It is based on using thunks to delay evaluation.  Part of this code
is adapted from code found at a Cornell University web site.*)

(* The stream type below allows finite and infinite streams.  Finite streams end with an 
end-of-stream marker that is written as Eos.  An infinite stream does not end so it will not have
an end-of-stream marker. *)
type 'a stream = Eos | StrCons of 'a * (unit -> 'a stream);;

(* Deconstructors for streams.  *)
let hdStr (s: 'a stream) : 'a =
  match s with
  | Eos -> failwith "headless stream"
  | StrCons (x,_) -> x;;

let tlStr (s : 'a stream) : 'a stream =
  match s with
  | Eos -> failwith "empty stream"
  | StrCons (x, t) -> t ();;
  
(* convert first n elements of a stream into a list, useful to display part of a stream. *)
let rec listify (s : 'a stream) (n: int) : 'a list =
  if n <= 0 then []
  else
    match s with
    | Eos -> []
    | _ -> (hdStr s) :: listify (tlStr s) (n - 1);;

(* n-th element of a stream *)
let rec nthStr (s : 'a stream) (n : int) : 'a =
  if n = 0 then hdStr s else nthStr (tlStr s) (n - 1);;

(* make a stream from a list *)
let from_list (l : 'a list) : 'a stream =
  List.fold_right (fun x s -> StrCons (x, fun () -> s)) l Eos;;

let rec ones = StrCons (1, fun () -> ones);;

let rec nums_from n = StrCons(n, fun () -> nums_from (n + 1));;

let nats = nums_from 0;;

let rec mapStr (f : 'a -> 'b) (s : 'a stream) : 'b stream =
  match s with
  | Eos -> Eos
  | _ -> StrCons (f (hdStr s), fun () -> mapStr f (tlStr s));;

let squares = mapStr (fun n -> n * n) nats;;

let rec filterStr (test : 'a -> bool) (s : 'a stream) : 'a stream =
  match s with
  | Eos -> Eos
  | StrCons (x, g) ->
      if (test x) then StrCons (x, fun () -> filterStr test (g ()))
      else filterStr test (g ());;

let rec map2Str (f: 'a -> 'b -> 'c)
             (s : 'a stream) (t : 'b stream) : 'c stream =
  match (s, t) with
  | (Eos, Eos) -> Eos
  | (StrCons (x, g), StrCons (y, h)) ->
       StrCons (f x y, fun () -> map2Str f (g ()) (h ()))
  | _ -> failwith "map2";;

let fibStr : int stream =
  let rec fibgen (a: int) (b: int) =
    StrCons (a, fun () -> fibgen b (a + b))
  in
  fibgen 1 1;;

let sift (p : int) : int stream -> int stream =
  filterStr (fun n -> n mod p <> 0);;

(* sieve of Eratosthenes *)
let rec sieve (s : int stream) : int stream =
  match s with
  | Eos -> Eos
  | StrCons (p, g) -> StrCons (p, fun () -> sieve (sift p (g ())));;

let primes = sieve (nums_from 2);;

let rec zip (s : 'a stream) (t : 'a stream) : 'a stream =
  match s with
  | Eos -> t
  | StrCons (x, g) -> StrCons (x, fun () -> zip t (g ()));;

let foo = zip fibStr primes;;

let rec unzip (s : 'a stream) : 'a stream * 'a stream =
  match (listify s 2) with
  | [] -> (Eos, Eos)
  | [x] -> (StrCons (x, fun () -> Eos), Eos)
  | x :: y :: _ ->
     let t = tlStr (tlStr s) in
     (StrCons (x, fun () -> fst (unzip t)), StrCons (y, fun () -> snd (unzip t)));;

let addStr (s1:int stream) (s2:int stream) = map2Str (fun x -> fun y -> x + y) s1 s2;;

let rec partial_sums (s : int stream) =
  match s with
  | Eos -> Eos
  | StrCons (h,t) -> StrCons (h, fun () -> (addStr (t ()) (partial_sums s)));;

(* An alternative definition of the natural numbers. *)
let altnats = partial_sums ones;;

let triangular = partial_sums nats;;

(* Pascal's triangle as an infinite stream of infinite streams.  *)
let rec pascal = StrCons(ones, (fun () -> (mapStr partial_sums pascal)));;

let row7 = nthStr pascal 7;;

listify row7 10;;

(* Infinite "decimal" expansion to any base. *)

let rec expand num den radix =
  StrCons (((num * radix) / den), fun () -> (expand ((num * radix) mod den) den radix));;

(* Searching for twin primes *)
let search_twins (s: int stream) =
  let rec helper (current: int) (str: int stream) =
    match str with
    | Eos -> Eos
    | StrCons (h, t) ->
       let next = h in
       if (next = current + 2) then
         StrCons((current,next), fun () -> helper next (t ()))
       else
         helper next (t ())
  in
  match s with
  | Eos -> Eos
  | StrCons(h,t) -> helper h (t ());;


let twin_primes = search_twins primes;;

listify twin_primes 10;;

(* Stream solution to the Hamming-Dijkstra problem. *)
let rec ordered_merge (s1: int stream) (s2: int stream) =
  match s1 with
  | Eos -> s2
  | StrCons(h1,t1) ->
     match s2 with
     | Eos -> s1
     | StrCons(h2, t2) ->
        if h1 = h2 then
          StrCons(h1, fun () -> ordered_merge (t1 ()) (t2 ()))
        else
          if h1 < h2 then
            StrCons(h1, fun () -> ordered_merge (t1 ()) s2)
          else
            StrCons(h2, fun () -> ordered_merge s1 (t2 ()));;

let scale_stream n s = mapStr (fun x -> n * x) s;;

let rec ham = StrCons(1,
                      fun () ->
                      ordered_merge (scale_stream 2 ham)
                        (ordered_merge (scale_stream 3 ham) (scale_stream 5 ham)));;

(* using delay and force as build in primitives *)

open Lazy;;

type 'a stream = Eos | StrCons of 'a * ('a stream t);;

type lazyint = int t;;

let a = lazy 3;;

force a;;

let hdStr (s: 'a stream) : 'a =
  match s with
  | Eos -> failwith "headless stream"
  | StrCons (x,_) -> x;;

let tlStr (s : 'a stream) : 'a stream =
  match s with
  | Eos -> failwith "empty stream"
  | StrCons (x, xs) -> force xs;;

(* convert first n elements of a stream into a list, useful to display part of a stream. *)
let rec listify (s : 'a stream) (n: int) : 'a list =
  if n <= 0 then []
  else
    match s with
    | Eos -> []
    | _ -> (hdStr s) :: listify (tlStr s) (n - 1);;

(* n-th element of a stream *)
let rec nthStr (s : 'a stream) (n : int) : 'a =
  if n = 0 then hdStr s else nthStr (tlStr s) (n - 1);;

(* make a stream from a list *)
let from_list (l : 'a list) : 'a stream =
  List.fold_right (fun x s -> StrCons (x, lazy s)) l Eos;;

let rec ones = StrCons (1, lazy ones);;

let rec nums_from n = StrCons(n, lazy (nums_from (n + 1)));;

let nats = nums_from 0;;

let rec mapStr (f : 'a -> 'b) (s : 'a stream) : 'b stream =
  match s with
  | Eos -> Eos
  | _ -> StrCons (f (hdStr s), lazy(mapStr f (tlStr s)));;

let squares = mapStr (fun n -> n * n) nats;;

let rec filterStr (test : 'a -> bool) (s : 'a stream) : 'a stream =
  match s with
  | Eos -> Eos
  | StrCons (x, g) ->
      if (test x) then StrCons (x, lazy(filterStr test (force g)))
      else filterStr test (force g);;

let rec map2Str (f: 'a -> 'b -> 'c)
             (s : 'a stream) (t : 'b stream) : 'c stream =
  match (s, t) with
  | (Eos, Eos) -> Eos
  | (StrCons (x, g), StrCons (y, h)) ->
       StrCons (f x y, lazy(map2Str f (force g) (force h)))
  | _ -> failwith "map2";;

let fibStr : int stream =
  let rec fibgen (a: int) (b: int) =
    StrCons (a, lazy(fibgen b (a + b)))
  in
  fibgen 1 1;;

let sift (p : int) : int stream -> int stream =
  filterStr (fun n -> n mod p <> 0);;

(* sieve of Eratosthenes *)
let rec sieve (s : int stream) : int stream =
  match s with
  | Eos -> Eos
  | StrCons (p, g) -> StrCons (p, lazy(sieve (sift p (force g))));;

let primes = sieve (nums_from 2);;

let rec zip (s : 'a stream) (t : 'a stream) : 'a stream =
  match s with
  | Eos -> t
  | StrCons (x, g) -> StrCons (x, lazy(zip t (force g)));;

let rec unzip (s : 'a stream) : 'a stream * 'a stream =
  match (listify s 2) with
  | [] -> (Eos, Eos)
  | [x] -> (StrCons (x, lazy Eos), Eos)
  | x :: y :: _ ->
     let t = tlStr (tlStr s) in
     (StrCons (x, lazy(fst (unzip t))), StrCons (y, lazy(snd (unzip t))));;

let addStr (s1:int stream) (s2:int stream) = map2Str (fun x -> fun y -> x + y) s1 s2;;

let rec partial_sums (s : int stream) =
  match s with
  | Eos -> Eos
  | StrCons (x,xs) -> StrCons (x, lazy(addStr (force xs) (partial_sums s)));;

(* An alternative definition of the natural numbers. *)
let altnats = partial_sums ones;;

let triangular = partial_sums nats;;

(* Pascal's triangle as an infinite stream of infinite streams.  *)
let rec pascal = StrCons(ones, lazy(mapStr partial_sums pascal));;

let row7 = nthStr pascal 7;;

listify row7 10;;

let rec expand num den radix =
  StrCons (((num * radix) / den), lazy(expand ((num * radix) mod den) den radix));;

(* Searching for twin primes *)
let search_twins (s: int stream) =
  let rec helper (current: int) (str: int stream) =
    match str with
    | Eos -> Eos
    | StrCons (x, xs) ->
       if (x = current + 2) then
         StrCons((current, x), lazy(helper x (force xs)))
       else
         helper x (force xs)
  in
  match s with
  | Eos -> Eos
  | StrCons(x,xs) -> helper x (force xs);;


let twin_primes = search_twins primes;;

listify twin_primes 10;;

(* Stream solution to the Hamming-Dijkstra problem. *)
let rec ordered_merge (s1: int stream) (s2: int stream) =
  match s1 with
  | Eos -> s2
  | StrCons(h1, t1) ->
     match s2 with
     | Eos -> s1
     | StrCons(h2, t2) ->
        if h1 = h2 then
          StrCons(h1, lazy(ordered_merge (force t1) (force t2)))
        else
          if h1 < h2 then
            StrCons(h1, lazy(ordered_merge (force t1) s2))
          else
            StrCons(h2, lazy(ordered_merge s1 (force t2)));;

let scale_stream n s = mapStr (fun x -> n * x) s;;

let rec ham = StrCons(1,
                      lazy(ordered_merge (scale_stream 2 ham)
                        (ordered_merge (scale_stream 3 ham) (scale_stream 5 ham))));;

let rec addFloatStreams (s1: float stream) (s2: float stream) =
  StrCons(((hdStr s1) +. (hdStr s2)), lazy((addFloatStreams (tlStr s1) (tlStr s2))));;

let rec onesFloat = StrCons(1.0, lazy onesFloat);;
let rec twosFloat = StrCons(2.0, lazy twosFloat);;

let integral integrand (init:float) (dt:float) = 
  let rec answer =
    StrCons(init, lazy(addFloatStreams (mapStr (fun (u : float) -> u *. dt) integrand) answer))
  in
    answer;;

let intOnes = integral onesFloat 0.0 0.1;;

let scale (sigma: float stream) (x: float) = mapStr (fun u -> (u *. x)) sigma;;

type signal = float stream
                    
let rc_circuit (r: float) (c: float) (dt: float) (inputSig : signal) (v0:float) = 
       (addFloatStreams (scale inputSig r)
                        (integral (scale inputSig (1.0 /. c)) v0 0.1));;

let current = mapStr (fun i -> sin (float(i) *. 0.1)) nats;;

let example_circuit = rc_circuit 5.0 1.0 0.1 current 0.0;;
