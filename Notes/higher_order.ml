(* Examples on increasing levels of abstraction *)

let rec sumInts(a,b) = if (a > b) then 0 else a + sumInts(a+1,b);;

let rec sumSquares(a,b) = if (a > b) then 0 else (a*a) + sumSquares(a+1,b);;
let rec sumCubes(a,b) = if (a > b) then 0 else (a*a*a) + sumCubes(a+1,b);;

let rec sum(f,lo,hi) =
    if (lo > hi) then 0
    else (f lo) + sum(f,lo+1,hi);;

let square n = n * n;;
let cube n = n * n * n;;

let rec sumInc(f,lo,hi,inc) =
    if (lo > hi) then 0
    else (f lo) + sumInc(f, (inc lo), hi, inc);;

let byTwo n = n + 2;;

let rec product(f,lo,hi,inc) =
    if (lo > hi) then 1
    else (f lo) * product(f, (inc lo), hi, inc);;

let id x = x;;
let inc n = n + 1;;
product(id, 1, 5, inc);;

let acc(comb,f,lo,hi,inc,unit) =
  let rec helper(a, acc) =
    if (a > hi) then acc
    else
      helper((inc a), comb(acc, (f a)))
    in
        helper(lo, unit);;

(* The following computes sum of the squares of numbers from 1 to 5 *).

acc((fun (n,m) -> n + m), (fun x -> x * x), 1, 5, (fun n -> n + 1), 0);;


(* Examples on higher-order functions from calculus *)

(* Doubler and self-application. *)
let twice f = fun x -> f (f x);;

let inc n = n + 1;;

let fourtimes f = (twice twice) f;;

let compose(f,g) = fun x -> g (f x);;

(* Some examples from calculus. *)
let deriv((f: float -> float), (dx:float)) = fun (x:float) -> ((f(x +. dx) -. f(x))/.dx);;

let absFl(x:float) = if (x < 0.0) then -.x else x;; 

let iterSum(f, (lo:float), (hi:float), inc) =
  let rec helper((x:float), (result:float)) =
    if (x > hi) then result
    else helper((inc x), (f x) +. result)
  in
  helper(lo,0.0);;

let integral((f: float -> float),(lo:float),(hi:float),(dx:float)) =
  let delta (x:float) = x +. dx in
  dx *. iterSum(f,(lo +. (dx/.2.0)), hi, delta);;

let r_sq (x:float):float = x *. x;;

integral(r_sq,0.0,1.0,0.001);;

integral(sin,0.0, 3.14159, 0.001);;

(* All code including pipes below *)

let rec sumInts(a,b) = if (a > b) then 0 else a + sumInts(a+1,b);;

let rec sumSquares(a,b) = if (a > b) then 0 else (a*a) + sumSquares(a+1,b);;
let rec sumCubes(a,b) = if (a > b) then 0 else (a*a*a) + sumCubes(a+1,b);;

let rec sum(f,lo,hi) =
    if (lo > hi) then 0
    else (f lo) + sum(f,lo+1,hi);;

let square n = n * n;;
let cube n = n * n * n;;

let rec sumInc(f,lo,hi,inc) =
    if (lo > hi) then 0
    else (f lo) + sumInc(f, (inc lo), hi, inc);;

let byTwo n = n + 2;;

let rec product(f,lo,hi,inc) =
    if (lo > hi) then 1
    else (f lo) * product(f, (inc lo), hi, inc);;

let id x = x;;
let inc n = n + 1;;
product(id, 1, 5, inc);;

let acc(comb,f,lo,hi,inc,unit) =
  let rec helper(a, acc) =
    if (a > hi) then acc
    else
      helper((inc a), comb(acc, (f a)))
    in
        helper(lo, unit);;

(* The following computes sum of the squares of numbers from 1 to 5 *).

acc((fun (n,m) -> n + m), (fun x -> x * x), 1, 5, (fun n -> n + 1), 0);;

(* Some examples from calculus *)

let deriv(f, dx) = fun x -> ((f(x +. dx) -. f(x))/.dx);;

let iterSum(f, lo, hi, inc) =
  let rec helper(x, result) =
    if (x > hi) then result
    else helper((inc x), (f x) +. result)
  in
  helper(lo,0.0);;

let integral(f,lo,hi,dx) =
  let delta x = x +. dx in
  dx *. iterSum(f,(lo +. (dx/.2.0)), hi, delta);;

let r_sq x = x *. x;;

integral(r_sq,0.0,1.0,0.001);;

integral(sin,0.0, 3.14159, 0.001);;

(* Some smoothing functions. *)
let smooth f delta =
  fun x -> ((f (x -. delta)) +. (f x) +. (f (x +. delta)))/.3.0;;

let smooth2 f dx = fun x -> ((0.1 *. (f (x -. (2.0 *. dx)))) +. (0.2 *. (f (x -. dx)))
                            +. (0.4 *. (f  x)) +. (0.2 *. (f (x +. (2.0 *. dx)))) +.
                              (0.1 *. (f (x +. (2.0 *. dx)))));;

(* Pipes in OCaml *)
let inc n = n + 1;;
let (--) lo hi = List.init (hi - lo + 1) (fun n -> n + lo);;

[1;2;3] |> List.map inc;;

(1 -- 10) |> List.fold_left (+) 0;;

(1 -- 5) |> List.map (fun n -> n * n) |> List.fold_left (+) 0;;
                               
let compose f g = fun x -> x |> f |> g;;

let (<|) f v = f v;;

(*  @@ is predefined in OCaml to be the same as <| as I just defined it. *)
