(* loading the prelude...*)
exception NotImplemented
type 'a tree = Empty | Node of 'a tree * 'a * 'a tree
val deriv : (float -> float) * float -> float -> float = <fun>
val iterSum : (float -> float) * float * float * (float -> float) -> float =
  <fun>
val integral : (float -> float) * float * float * float -> float = <fun>


(* Question 1 *)

let mapTree_tests =
  [
    (((fun x -> x+1), Node(Empty, 0, (Node(Empty, 1, Empty)))),Node(Empty, 1, (Node(Empty, 2, Empty))));
    (((fun x -> x+1), Node(Node(Empty,5,Empty),6,Empty)),Node(Node(Empty,6,Empty),7,Empty));
    
    (((fun x -> x + 1),Empty),Empty);
  ]

let rec mapTree (f, (t: 'a tree)) =
  match t with
  | Empty -> Empty
  | Node (l,m,r) -> Node ((mapTree (f, l)),(f m),(mapTree (f, r))) 
;;

(* Question 2. *)

let halfint_tests =
  [
    (((fun x -> x),5.0,-5.0,0.00001) , 0.0);
    (((fun x -> x+.1.0),5.0,-5.0,0.000000000000000000000001) , -1.0);
  ]

let rec halfint ((f: float -> float), (posValue : float), (negValue : float), (epsilon : float)) =
  let mid = (negValue +. posValue) /. 2.0 in 
  if abs_float (f mid) < epsilon then mid
  else if (f mid) < 0.0 then halfint (f,posValue, mid, epsilon)
  else halfint (f,mid, negValue, epsilon) 
;;

(* Question 3. *)

let newton_tests =
  [
    ((sin,5.0,0.0001,0.0001),9.42477);
  ]

let rec newton ((f: float -> float),  (guess:float), (epsilon:float), (dx:float)) =
  let close((x:float), (y:float), (epsilon:float)) = abs_float(x-.y) < epsilon in
  (*let improve((guess:float),f,(dx:float)) = raise NotImplemented in*)
  if close((f guess), 0.0, epsilon)
  then
    guess
  else
    newton(f ,(guess -. (f guess)/. (deriv(f,dx) guess)),epsilon,dx) 
      
      
;;

(* Question 4. *)

let indIntegral (f, (dx:float)) =
  fun x -> integral (f,0.0,x,dx) 
;;

