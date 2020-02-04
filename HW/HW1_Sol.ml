(* Question 1 Square root. *)
open Float;;

let close((x:float), (y:float)) = (abs(x -. y) < 0.0001);;

let square(x:float) = x *. x;;

let rec mysqrt((x:float), (guess:float)) =
    if close(square(guess), x) then guess
    else mysqrt(x, (guess +. x/.guess)/.2.0);;

(* Question 2 Cube root.  *)

let cube(x:float) = x*. x*. x;;

let rec cube_root((x:float), (guess:float)) =
  if close(cube(guess),x) then guess
  else cube_root(x, ((2.0 *. guess) +. x/.(square(guess)))/.3.0);;

(* Question 3 Tail-recursive version of fast-exponentiation.  *)

let odd n = (n mod 2) = 1;;

let rec fast_exp_aux (base, power, acc) =
  if power = 0 then acc
  else
    if (odd power) then
      fast_exp_aux (base, power - 1, base * acc)
    else
      fast_exp_aux (base * base, power / 2, acc);;

let fast_exp (base, power) =
  if base = 0 then 0
  else if power = 0 then 1
  else
    fast_exp_aux(base, power, 1);;
