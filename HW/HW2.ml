(* loading the prelude...*)
exception NotImplemented
val sumlist : float list -> float = <fun>


(* Q1a TODO: Write your own tests for the pairlists function.
         You should NOT test lists of different lengths.
*)
let pairlists_tests = [
  (* Your test cases go here. *)
  (([1;2;3],[2;3;4]),[(1,2);(2,3);(3,4)]);
  (([],[]),[]);
]

(* Q1a TODO: Implement pairlists. *)
let rec pairlists (l1,l2) =
  match (l1,l2) with
  | [],[] -> []
  | x::xs, y::ys -> (x,y)::pairlists (xs, ys)
;;

(* Q1b TODO: Write your own tests for the w_mean function.
         You should NOT test lists of different lengths.
*)
let w_mean_tests = [
  (* Your test cases go here. *)
  (([1.0],[-10.0]),-10.0); 
  (([1.0;2.0;3.0],[1.0;2.0;3.0]),14.0/.6.0); 
  
  
]


(* Q1b TODO: Implement w_mean. *) 

let w_mean weights data = 
  (sumlist (List.map (fun (x,y) -> x*.y) (pairlists (weights,data)))) /.
  (sumlist (weights))
    
;;

(* Q2 TODO: Write your own tests for the memberof function. *)
let memberof_tests = [
  (* Your test cases go here. *)
  ((2,[2;3;4]),true);
  ((0,[2;3;4]),false);
  ((0,[]),false);
]

(* Q2 TODO: Implement memberof. *)
let rec memberof (a,l) =
  match l with
  | [] -> false
  | x::xs -> if(a==x) then true
      else memberof (a,xs)
;;

(* Q2 TODO: Write your own tests for the remove function. *)
let remove_tests = [
  (* Your test cases go here. *)
  ((2,[2;3;4]),[3;4]);
  ((5,[2;3;4]),[2;3;4]);
  ((2,[]),[]);
  ((2,[2;2;2]),[]);
]

(* Q2 TODO: Implement remove. *)
let rec remove (item, lst) = 
  match lst with
  | [] -> []
  | x :: xs -> if(x==item) then remove(x,xs)
      else x :: (remove (item ,xs))
                
;;

(* Q3 TODO: Write your own tests for the find_max function. *)
let find_max_tests = [
  (* Your test cases go here. *)
  ([2;3;4],4);
  ([2;2;2],2);
  ([-2;-4;-5],-2);
]

(* Q3 TODO: Implement find_max. *)
let find_max l =
  let x::xs = l  in
  List.fold_left max x xs
;;

(* Q4 TODO: Write your own tests for the selsort function. *)
let selsort_tests = [
  (* Your test cases go here. *)
  ([1; 4; 2; 5; 3; 9; 6; 8; 7],[9; 8; 7; 6; 5; 4; 3; 2; 1]);
  ([-1; -4; -2],[-1;-2;-4]);
  ([],[]);
]

(* Q4 TODO: Implement selsort. *)
let rec selsort l =
  match l with 
  |[] ->[]
  | x::xs -> find_max l :: selsort ( (remove (find_max l, l)))
                                       
;;
