(* Question 1. *)

let rec sumlist l =
  match l with
  | [] -> 0.0 
  | (x::xs) -> x +. sumlist(xs);;

let pairlists_tests = [
  (([],[]), []);
  (([1;2;3], [4;5;6]), [(1, 4);(2, 5);(3, 6)])
]

let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([],x::xs) -> failwith "Error -- lists are not of the same length"
    | (x::xs, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y) :: (pairlists (xs,ys));;

let w_mean_tests = [
  (([1.0],[1.0]), 1.0);
  (([1.0; 2.0], [-1.0;2.0]), 1.0)
]

let w_mean weights data =
  let denom = sumlist weights in
  let pairs = pairlists (weights, data) in
  (sumlist (List.map (fun (x,y) -> x *. y) pairs))/.denom;;
  
w_mean [1.0;1.5;2.5;0.5;1.5] [10.3;11.7;2.0;5.0;6.5];;

(* Question 2. *)

let memberof_tests = [
  ((1, [2;3;4]), false);
  ((1, []), false);
  ((2, [1;3;2;3]), true)
]

let rec memberof pair =
  match pair with
  | (n,[]) -> false
  | (n,(x::xs)) -> if (x = n) then true else memberof(n,xs);;

let remove_tests = [
  ((1, [2;3;1;4]), [2;3;4]);
  ((1, []), []);
  ((1,[2;1;2;1;2]), [2;2;2]);
  ((1, [2;3;4]), [2;3;4]);
]

let rec remove(item, lst) = 
  match lst with
  | [] -> []
  | (x::xs) -> if (x = item) then remove(item, xs) else x::(remove(item, xs));;

(* Question 3. *)

let find_max_tests = [
  ([1;2;3], 3);
  ([3;2;1], 3);
  ([-2;0;1], 1)
]

let find_max l =
  let rec helper(l,m) =
    match l with
      | [] -> m
      | (x::xs) ->
         if (m < x) then helper(xs,x) else helper(xs,m)
  in
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x);;

(* Question 4. *)

let selsort_tests = [
  ([], []);
  ([1;2;3], [3;2;1]);
  ([3;0;-1;2], [3;2;0;-1])
]
  
let rec selsort l =
  match l with
  | [] -> []
  | _ -> let m = (find_max l) in
         m::(selsort(remove(m,l)));;
