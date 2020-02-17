(* The basic list operations are "cons" written as :: which adds an element
to the front of a list and "append", written @ which combines two lists.
Lists in OCaml are homogenous and immutable.  List items are separated by semi-colons.
*)

let vowels = ['a';'e';'i';'o';'u']

let vowels2 = 'y' :: vowels

(* What if we want to add y at the end? vowels :: 'y' gives a type error. *)

let vowels3 = vowels @ ['y']
(* We had to make 'y' into a list (of length 1) by writing ['y']. *)

(* How do we take lists apart?  OCaml provides "destructors" to take a list apart.  
They are called "hd" and "tl" for "head" and "tail". *)
let liszt = [1; 2; 3; 4; 5]

let liszt2 = [11; 12; 13; 14; 15]

(* These are discouraged.  Use pattern-matching instead.  *)
let v = List.hd liszt

let t = List.tl liszt

let rec badzip (l1,l2) =
  if l1 = [] then l2
  else
    (List.hd(l1))::(badzip (l2,List.tl(l1)))

let foo = badzip(liszt,liszt2);;
    

(* The pattern matching version. *)

let rec zip (l1, l2) =
  match l1 with
  | [] -> l2
  | x:: xs -> x :: zip(l2, xs);;

zip([1;3;5],[2;4;6]);;

(* Our own append function. It is O(n), unlike cons which is O(1). *)
let rec myappend l1 l2 = 
  match l1 with
  | [] -> l2
  | x :: xs -> x :: (myappend xs l2);;


(* Reverse done naively.  This is O(n^2). *)
let rec reverse l = 
  (match l with    
  | [] -> []
  | x::xs -> (reverse xs) @ [x]);;


(* Better reverse using an accumulating parameter. This is O(n). *)
let rev l = 
  let rec helper(l,acc) = 
    match l with
    | [] -> acc
    | x :: xs -> helper(xs, x::acc)
  in
  helper(l,[]);;

(* Some functions in the List module *)
open List;;

let foo = init 20 (fun n -> n);;

concat [[1;2;3];[4;5;6];[7;8;9]];;

nth foo 7;;

(* A string to list function; not provided in OCaml. *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;


(*  Below are the results of running the above definitions and expressions through the interpreter.

val vowels : char list = ['a'; 'e'; 'i'; 'o'; 'u']
val vowels2 : char list = ['y'; 'a'; 'e'; 'i'; 'o'; 'u']
val vowels3 : char list = ['a'; 'e'; 'i'; 'o'; 'u'; 'y']
val liszt : int list = [1; 2; 3; 4; 5]
val liszt2 : int list = [11; 12; 13; 14; 15]
val v : int = 1
val t : int list = [2; 3; 4; 5]
val badzip : 'a list * 'a list -> 'a list = <fun>
val foo : int list = [1; 11; 2; 12; 3; 13; 4; 14; 5; 15]
val zip : 'a list * 'a list -> 'a list = <fun>
#   - : int list = [1; 2; 3; 4; 5; 6]
val myappend : 'a list -> 'a list -> 'a list = <fun>
val reverse : 'a list -> 'a list = <fun>
val rev : 'a list -> 'a list = <fun>
#   val foo : int list =
  [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19]
#   - : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9]
#   - : int = 7
val explode : string -> char list = <fun>

 *)


(* Example of insert and insertion sort *)

let rec insert(n, l) =
  match l with
  | [] ->  [n]
  | x::xs ->
     if (n < x)
     then
       n::(x::xs)
     else
       x::(insert(n,xs));;

insert (3,[1;2;4;5]);;

let rec isort l =
  match l with
  | [] -> []
  | x::xs -> insert(x, isort(xs));;
           
isort [8;2;5;1;3;2;6;9;7];;


(* More list examples *)

let rec mymap f l =
  match l with
  | [] -> []
  | item :: rest -> (f item) :: (mymap f rest);;

mymap (fun n -> n + 1) [1;2;3];;

let odd n = (n mod 2) = 1;;

let l1 = List.init 9 (fun n -> n * n);;
          
let rec myfilter test l =
  match l with
  | [] -> []
  | x :: xs -> if (test x) then x :: (myfilter test xs)
               else (myfilter test xs);;

let l2 = myfilter odd l1;;

(* map and filter are built in.  There is a special library (such libraries are called 
"modules" in OCaml.)  To use the built-in functions write List.map or List.filter.  Or you can write 
"open List" as I have done below and the List library functions will be available.  *)

open List;;

filter odd l1;;
map (fun n -> n * n * n) (init 10 (fun n -> n));;

(* The most useful function in the List module. *)

(* First I will write it myself *)
let rec my_fold_left (f : 'a -> 'b ->'a) (acc : 'a) (l : 'b list): 'a =
  match l with
  |  [] -> acc
  | x :: xs -> my_fold_left f (f acc x) xs

let sum lst =
  fold_left (+) 0 lst;;

let myconcat lstlst = fold_left (@) [] lstlst;;

let stringmash strlst = fold_left (fun (s1:string) -> fun (s2:string) -> s1 ^ s2) "" strlst;;

(* Almost equally useful: fold_right: ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)

let rec my_fold_right (f : 'a -> 'b -> 'b) (l : 'a list) (acc : 'b) : 'b =
  match l with
  |  [] -> acc
  | x :: xs -> f x (my_fold_right f xs acc);;

let newstringmash strlst = fold_right (fun s1 -> fun s2 -> s1 ^ s2) strlst "";;

(* We can write many things in terms of folds. *)
let len l = fold_left (fun a _ -> a + 1) 0 l;;
let rev l = fold_left (fun a x -> x :: a) [] l;;
let newmap f l = fold_right (fun x a -> (f x) :: a) l [];;
let newfilter f l = fold_right (fun x a -> if f x then x :: a else a) l [];;


(* Tricky examples on lists *)

let psums lst =
  let rec helper l a =
    match l with
      | [] -> [a]
      | x ::xs -> a :: (helper xs (x + a))
  in
  match lst with
    | [] -> [0]
    | x :: xs -> helper xs x;;

let smash ll = List.fold_left (@) [] ll;;

let rec inter item lst =
  match lst with
    | [] -> [[item]]
    | x :: xs -> (item :: lst) :: (List.map (fun u -> (x:: u)) (inter item xs));;

let rec perms l =
  match l with
    | [] -> [[]]
    | x::xs -> smash (List.map (fun u -> (inter x u)) (perms xs));;

