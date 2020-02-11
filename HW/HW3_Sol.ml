(* Question 1. *)

let common_tests = [
    (([],[]), []);
    (([],[1;2;3]), []);
    (([1;2;3],[]), []);
    (([6;2;4;1;5;6],[9;2;3;4;6]), [6; 2; 4])
  ]

let rec common twolists =
  match twolists with
    | (l,[]) -> []
    | ([],l) -> []
    | ((x::xs),l) ->
        if (List.mem x l)
        then
          x::(common(xs, (List.filter (fun u -> not(u = x)) l)))
        else
          common(xs,l);;

(* Question 2. The next three questions are about mergesort.  Mergesort
requires that you use recursion.  Using isort or some other sort defeats
the whole purpose.  This question asks for the implementation of split.*)

let split_tests = [
    ([], ([], []));
    ([0;1;2;3], ([0;2], [1;3]));
    ([1;2;3], ([1;3], [2]))
  ]

let rec split l =
  match l with
  | [] -> ([],[])
  | x :: [] -> (x::[],[])
  | (x::y::l) ->
  let (odds,evens) = split(l)
  in
      (x::odds,y::evens);;

(* Question 3 Here you implement merge. *)

let merge_tests = [
    (([],[]), []);
    (([],[1;2;3]), [1;2;3]);
    (([1;2;3],[]), [1;2;3]);
    (([1;3;5],[2;8]), [1;2;3;5;8])
  ]

let rec merge twolists =
  match twolists with
  | ([],rt) -> rt
  | (lft,[]) -> lft
  | (x::xs, y::ys) ->
    if (x < y)
    then
      x::merge(xs, y::ys)
    else
      y::merge(x::xs,ys);;

(* Question 4 Finally you combine split and merge and use them to implement mergesort. *)

let mergesort_tests = [
    ([], []);
    ([1], [1]);
    ([9;7;9;5;6], [5;6;7;9;9])
  ]

let rec mergesort l =
  match l with
  | [] -> []
  | (n::[]) -> n::[] (* Without this you will go into an infinite loop. *)
  | n::ns ->
    let (m,n) = split(l) in
      let m2 = mergesort(m) in
      let n2 = mergesort(n) in
      merge(m2,n2);;
