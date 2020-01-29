(* Question 1. *)

let common_tests = [
  (([1;2;3],[2;3;4]),[2;3]);
  (([],[]),[]);
  (([2],[]),[]);
  (([],[4]),[]);
  (([2;2;2],[2;3;4]),[2]);
]

let rec common (l1,l2) =
  match l1 with
  | [] -> []
  | x::xs -> 
      if List.mem x l2 then x::common(xs,remove(x,l2))
      else common(xs,l2)
;;

(* Question 2. Mergesort requires that you use recursion.  Using List.sort or
some other sort defeats the whole purpose.  This question is for the
implementation of split.*)

let split_tests = [
  ([1;2;3;4;5;6],([1;3;5],[2;4;6]));
  ([],([],[]));
  ([1;2;3],([1;3],[2]));
]

let rec split l =
  match l with
  |[] -> ([],[])
  |x::[] -> ([x],[])
  |x1::x2::xs -> let (l1,l2) = split xs in
      (x1::l1,x2::l2)
;;

(* Question 3 Here you implement merge. *)

let merge_tests = [
  (([1;3;5;7;9],[2;4;6;8]),[1;2;3;4;5;6;7;8;9]);
  (([],[]),[]);
  (([1;2;3],[4;5;6]),[1;2;3;4;5;6]);
  (([1;2;3],[]),[1;2;3]);

]

let rec merge twolists =
  match twolists with
  |([],[]) -> []
  | (x, []) -> x
  | ([], y) -> y
  |(x::xs,y::ys) -> 
      if x<y then x::(merge (xs,y::ys))
      else y::(merge (x::xs,ys))
;;

(* Question 4 Finally you combine split and merge and use them to implement mergesort. *)

let mergesort_tests = [
  ([10;2;8;5;1;4;3;9;7;6],[1;2;3;4;5;6;7;8;9;10]);
  ([],[]);
  ([1],[1]);
]

let rec mergesort l =
  match l with 
  | [] -> [] 
  | (x::[]) -> x::[]
  | (x::xs) -> let (l1, l2) = split l in merge(mergesort l1, mergesort l2) 
;;
