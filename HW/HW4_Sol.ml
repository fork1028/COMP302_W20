(* Question 1 *)
type 'a tree = Empty | Node of 'a tree * 'a * 'a tree;;

let rec mapTree(f, (t: 'a tree)) =
  match t with
  | Empty -> Empty
  | Node(l,v,r) -> Node((mapTree(f,l)), (f v), (mapTree(f, r)));;

(* Question 2. *)
let abs_float (x:float) = if (x < 0.0) then (0.0 -. x) else x;;

let rec halfint((f: float -> float),(posValue:float),(negValue:float),(epsilon:float)) =
  let mid = (posValue +. negValue)/.2.0 in
  if ((abs_float(f mid)) < epsilon)
  then
    mid
  else
    if (f mid) < 0.0
    then
      halfint(f,posValue,mid,epsilon)
    else
    halfint(f,mid,negValue,epsilon);;

(* Question 3. *)
let deriv((f: float -> float), (dx: float)) = fun (x:float) -> (((f (x +. dx)) -. (f x))/.dx);;

let rec newton(f, (guess:float), (epsilon:float), (dx:float)) =
  let close((x:float), (y:float), (epsilon:float)) = abs_float(x-.y) < epsilon in
  let improve((guess:float),f,(dx:float)) = guess -. (f guess)/.((deriv(f,dx)) guess) in
  if close((f guess), 0.0, epsilon)
  then
    guess
  else
    newton(f,improve(guess,f,dx),epsilon,dx);;

(* For testing *)
let make_cubic((a:float),(b:float),(c:float)) = fun x -> (x*.x*.x +. a *. x*.x +. b*.x +. c)
let test1 = newton(make_cubic(2.0,-3.0,1.0),0.0,0.0001,0.0001)
(* Should get -3.079599812; don't worry if your last couple of digits are off. *)
let test2 = newton(sin,5.0,0.0001,0.0001)
(* Should get 9.424.... *)

(* Question 4. *)
(* The first two functions below should be preloaded into the environment. *)

  let iterSum(f, (lo:float), (hi:float), inc) =
    let rec helper((x:float), (result:float)) =
      if (x > hi) then result
      else helper((inc x), (f x) +. result)
    in
    helper(lo,0.0);;

let integral((f: float -> float),(lo:float),(hi:float),(dx:float)) =
  let delta (x:float) = x +. dx in
  dx *. iterSum(f,(lo +. (dx/.2.0)), hi, delta);;

(* This is all they have to write. *)

let indIntegral(f,(dx:float)) = fun (x:float) -> integral(f,0.0,x,dx);;
