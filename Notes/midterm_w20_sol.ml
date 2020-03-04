(* The second case is important; we took a few points off for missing each of the base cases.  One mistake
was calling remove_consec_dup more in the "then" clause below.  We took points off for that as well.  We gave
zero to people who removed all duplicates ignoring whether they came in consecutive blocks.  *)

let rec remove_consec_dup l =
  match l with
  | [] -> []
  | a::[] -> [a]
  | a ::(b :: more) -> if a = b
                       then
                         remove_consec_dup (b :: more)
                       else
                         a:: (remove_consec_dup (b :: more));;

(* A bunch of people got this easily and another larger bunch was quite lost.  In particular a number of
people were mystified about where the "value of y comes from".  Asking this question shows a lack of understanding
of higher-order functions as values.  You don't supply "y", you feed a function that has "y" as a paramemter. *)

(*  There are a number of different ways of writing this.  We gave full marks for anything that is equivalent 
to the version below.  I did not leave the dx as a parameter because I wanted to make the type look like
what the mathematical version would suggest.  The dx parameter is a numerical approximation parameter and I 
expected you  to choose some small number for it.  I did not mind if you choose anything small, but min-float 
is not small in absolute value.*)

let convolution f g = fun x -> integral((fun y -> f(y) *. g(x -. y)), min_float, max_float, 0.0001);;

(* The answer is 6.  *)

let x = 1 in
    let y = x + 1 in
    let rec f n = if n = 0 then 0 else y + (f (n - x)) in
    let x = 3 in f x;;
