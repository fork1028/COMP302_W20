# type 'a stream = Eos | StrCons of 'a * (unit -> 'a stream);;

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
type 'a stream = Eos | StrCons of 'a * (unit -> 'a stream)
#         val hdStr : 'a stream -> 'a = <fun>
#         val tlStr : 'a stream -> 'a stream = <fun>
#               val listify : 'a stream -> int -> 'a list = <fun>
#       val nthStr : 'a stream -> int -> 'a = <fun>
#       val from_list : 'a list -> 'a stream = <fun>
#     val ones : int stream = StrCons (1, <fun>)
#   val nums_from : int -> int stream = <fun>
#   val nats : int stream = StrCons (0, <fun>)
#         val mapStr : ('a -> 'b) -> 'a stream -> 'b stream = <fun>
#             val filterStr : ('a -> bool) -> 'a stream -> 'a stream = <fun>
#               val map2Str : ('a -> 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream = <fun>
#           val fibStr : int stream = StrCons (1, <fun>)
#     val sift : int -> int stream -> int stream = <fun>
#           val sieve : int stream -> int stream = <fun>
#   val primes : int stream = StrCons (2, <fun>)
#         val zip : 'a stream -> 'a stream -> 'a stream = <fun>
#   val foo : int stream = StrCons (1, <fun>)
#               val unzip : 'a stream -> 'a stream * 'a stream = <fun>
# listify primes 100;;
- : int list =
[2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71;
 73; 79; 83; 89; 97; 101; 103; 107; 109; 113; 127; 131; 137; 139; 149; 151;
 157; 163; 167; 173; 179; 181; 191; 193; 197; 199; 211; 223; 227; 229; 233;
 239; 241; 251; 257; 263; 269; 271; 277; 281; 283; 293; 307; 311; 313; 317;
 331; 337; 347; 349; 353; 359; 367; 373; 379; 383; 389; 397; 401; 409; 419;
 421; 431; 433; 439; 443; 449; 457; 461; 463; 467; 479; 487; 491; 499; 503;
 509; 521; 523; 541]
# 

# 
# 
# 
# let addStr (s1:int stream) (s2:int stream) = map2Str (fun x -> fun y -> x + y) s1 s2;;
val addStr : int stream -> int stream -> int stream = <fun>
# 
# 
# 
# let rec partial_sums (s : int stream) =
  match s with
  | Eos -> Eos
  | StrCons (h,t) -> StrCons (h, fun () -> (addStr (t ()) (partial_sums s)));;
      val partial_sums : int stream -> int stream = <fun>
# 
# let altnats = partial_sums ones;;
val altnats : int stream = StrCons (1, <fun>)
# listify altnats 10;;
- : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
# let triangular = partial_sums nats;;
val triangular : int stream = StrCons (0, <fun>)
# listify triangular 20;;
- : int list =
[0; 1; 3; 6; 10; 15; 21; 28; 36; 45; 55; 66; 78; 91; 105; 120; 136; 153; 171;
 190]
# 
# 
# 
# let rec pascal = StrCons(ones, (fun () -> (mapStr partial_sums pascal)));;
val pascal : int stream stream = StrCons (StrCons (1, <fun>), <fun>)
# let row7 = nthStr pascal 7;;
val row7 : int stream = StrCons (1, <fun>)
# listify row7 10;;
- : int list = [1; 8; 36; 120; 330; 792; 1716; 3432; 6435; 11440]
# 
# 
# 
# 
# let rec expand num den radix =
  StrCons (((num * radix) / den), fun () -> (expand ((num * radix) mod den) den radix));;
  val expand : int -> int -> int -> int stream = <fun>
# let qaz = expand 1 7 10;;
val qaz : int stream = StrCons (1, <fun>)
# listify qaz 6;;
- : int list = [1; 4; 2; 8; 5; 7]
# listify qaz 12;;
- : int list = [1; 4; 2; 8; 5; 7; 1; 4; 2; 8; 5; 7]
# listify (expand 1 10 7) 10;;
- : int list = [0; 4; 6; 2; 0; 4; 6; 2; 0; 4]
# listify (expand 1 17 10) 500;;
- : int list =
[0; 5; 8; 8; 2; 3; 5; 2; 9; 4; 1; 1; 7; 6; 4; 7; 0; 5; 8; 8; 2; 3; 5; 2; 9;
 4; 1; 1; 7; 6; 4; 7; 0; 5; 8; 8; 2; 3; 5; 2; 9; 4; 1; 1; 7; 6; 4; 7; 0; 5;
 8; 8; 2; 3; 5; 2; 9; 4; 1; 1; 7; 6; 4; 7; 0; 5; 8; 8; 2; 3; 5; 2; 9; 4; 1;
 1; 7; 6; 4; 7; 0; 5; 8; 8; 2; 3; 5; 2; 9; 4; 1; 1; 7; 6; 4; 7; 0; 5; 8; 8;
 2; 3; 5; 2; 9; 4; 1; 1; 7; 6; 4; 7; 0; 5; 8; 8; 2; 3; 5; 2; 9; 4; 1; 1; 7;
 6; 4; 7; 0; 5; 8; 8; 2; 3; 5; 2; 9; 4; 1; 1; 7; 6; 4; 7; 0; 5; 8; 8; 2; 3;
 5; 2; 9; 4; 1; 1; 7; 6; 4; 7; 0; 5; 8; 8; 2; 3; 5; 2; 9; 4; 1; 1; 7; 6; 4;
 7; 0; 5; 8; 8; 2; 3; 5; 2; 9; 4; 1; 1; 7; 6; 4; 7; 0; 5; 8; 8; 2; 3; 5; 2;
 9; 4; 1; 1; 7; 6; 4; 7; 0; 5; 8; 8; 2; 3; 5; 2; 9; 4; 1; 1; 7; 6; 4; 7; 0;
 5; 8; 8; 2; 3; 5; 2; 9; 4; 1; 1; 7; 6; 4; 7; 0; 5; 8; 8; 2; 3; 5; 2; 9; 4;
 1; 1; 7; 6; 4; 7; 0; 5; 8; 8; 2; 3; 5; 2; 9; 4; 1; 1; 7; 6; 4; 7; 0; 5; 8;
 8; 2; 3; 5; 2; 9; 4; 1; 1; 7; 6; 4; 7; 0; 5; 8; 8; 2; 3; 5; 2; 9; 4; 1; ...]
# let search_twins (s: int stream) =
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
                          val search_twins : int stream -> (int * int) stream = <fun>
# let twin_primes = search_twins primes;;
val twin_primes : (int * int) stream = StrCons ((3, 5), <fun>)
# 
# 
# 
# 
# listify twin_primes 10;;
- : (int * int) list =
[(3, 5); (5, 7); (11, 13); (17, 19); (29, 31); (41, 43); (59, 61); (71, 73);
 (101, 103); (107, 109)]
# let squares = mapStr (fun n -> n * n) nats;;
val squares : int stream = StrCons (0, <fun>)
# listify squares 20;;
- : int list =
[0; 1; 4; 9; 16; 25; 36; 49; 64; 81; 100; 121; 144; 169; 196; 225; 256; 289;
 324; 361]
# let rec ordered_merge (s1: int stream) (s2: int stream) =
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
                          val ordered_merge : int stream -> int stream -> int stream = <fun>
# let scale_stream n s = mapStr (fun x -> n * x) s;;
val scale_stream : int -> int stream -> int stream = <fun>
# 
# let rec ham = StrCons(1,
                      fun () ->
                      ordered_merge (scale_stream 2 ham)
                        (ordered_merge (scale_stream 3 ham) (scale_stream 5 ham)));;
      val ham : int stream = StrCons (1, <fun>)
# listify ham 15;;
- : int list = [1; 2; 3; 4; 5; 6; 8; 9; 10; 12; 15; 16; 18; 20; 24]
#   <menu-bar> <Tuareg> <Interactive Mode> <Kill OCaml REPL>
Process OCaml killed: 9
