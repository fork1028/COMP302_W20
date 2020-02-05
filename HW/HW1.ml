(* loading the prelude...*)
exception NotImplemented
val close : float * float -> bool = <fun>
val square : float -> float = <fun>
val cube : float -> float = <fun>
val odd : int -> bool = <fun>



(* Q1 TODO: Correct these tests for the double function. *)
let double_tests = [
  (0, 0);
  (1, 2);
  (3, 6); 
]

(* Q1 TODO: Correct this implementation so that it compiles and returns
the correct answers.
*)
let double int = match int with
  | n -> 2 * n 
  | 0 -> 0



(* Q1 TODO: Write your own tests for the fact function.
         See the provided tests for double, above, for how to write test cases.
Remember that you should NOT test cases for n < 0.
*)
let fact_tests = [
  (* Your test cases go here.
     Remember that the outputs of fact should be *floating-point* numbers.
  *)
  (1,1.0);
  (0,1.0);
  (2, 2.0);
  (3, 6.0);
  (4, 24.0);

]

(* Q1 TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let rec fact (n: int): float = match n with
  | 0 -> 1.0
  | _ -> ((float n)) *. fact (n - 1)

(* Q2 TODO: Write your own tests for the mysqrt function.
         You should NOT test cases for n < 0.
*)
let mysqrt_tests = [
  (* Your test cases go here. *)
  (1.0,1.0);
  (0.0,0.0) ;
  (4.0,2.0);
  (16.0,4.0);
]

(* Q2 TODO: Implement mysqrt. *)
let is_good_enough guess x =
  abs_float (guess *. guess -. x) < 0.0001
let improve guess x = (guess +. x /. guess) /. 2.0
let rec sqrt_iter guess x =
  if is_good_enough guess x then guess
  else sqrt_iter (improve guess x) x
      
let mysqrt (x:float) = sqrt_iter 1.0 x
  

(* Q3 TODO: Write your own tests for the cube_root function.
            You should NOT test cases for n < 0.
*)
let cube_root_tests = [
  (* Your test cases go here. *)
  (0.0,0.0);
  (1.0,1.0);
  (27.0,3.0);
]

(* Q3 TODO: Implement cube_root. *)
let is_good_enough guess x =
  abs_float (guess *. guess *. guess -. x) < 0.0001
  
let improve guess x = (2.0 *. guess +. x /. square(guess)) /. 3.0
                      
let rec cube_iter guess x =
  if is_good_enough guess x then guess
  else cube_iter (improve guess x) x
      
let cube_root (x:float) = cube_iter 1.0 x

(* Q4 TODO: Write your own tests for the fast_exp function.
            You should NOT test cases for negative bases or powers.
*)
let fast_exp_tests = [
  (* Your test cases go here. *)
  ((2,3),8);
  ((2,0),1);
  ((2,1),2);
  ((0,2),0);
]

(* Q4 TODO: Implement tail recursive helper fast_exp_aux. *)
let rec fast_exp_aux (base, power, acc) = 
  if power = 0 then acc
  else if base = 0 then 0
  else fast_exp_aux (base, power-1, base*acc)
      

(* Q4 TODO: Implement fast_exp using fast_exp_aux. *)
let fast_exp (base, power) = 
  fast_exp_aux(base,power,1)
                                         

                           

