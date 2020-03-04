(* Church numerals in OCaml *)

(* Church numerals can be typed with a recursive type *)
type church = Ch of ((church -> church) -> church -> church)

(* To apply a Church numeral as a function, remove the constructor Ch *)
let apply (Ch n) = n

(* Successor *)
let add1 (Ch n) = Ch (fun f x -> f (n f x))

let zero = Ch (fun f x -> x)
let one = add1 zero
let two = add1 one
let three = add1 two
let four = add1 three
let five = add1 four
let six = add1 five
let seven = add1 six
let eight = add1 seven
let nine = add1 eight
let ten = add1 nine

(* Arithmetic *)
let add n (Ch m) = m (add1) n
let mul n (Ch m) = m (add n) zero
let exp n (Ch m) = m (mul n) one

(* Test for zero *)
let if_zero (Ch n) x y = n (fun _ -> y) x

(* Pairs of numbers                           *)
(* these are of type church, but not numerals *)
let pair x y = Ch (fun _ -> fun z -> if_zero z x y)
let fst (Ch p) = p (fun x -> x) zero
let snd (Ch p) = p (fun x -> x) one

(* Proper subtraction *)
let next p = pair (snd p) (add1 (snd p))
let sub1 (Ch n) = fst (n next (pair zero zero))
let sub n (Ch m) = m sub1 n

(* Booleans *)
let true_ = one
let false_ = zero
let not_ x = if_zero x true_ false_
let and_ x y = mul x y
let or_ x y = not_ (and_ (not_ x) (not_ y))
let le x y = not_ (sub x y)
let lt x y = not_ (le y x)
let eq x y = (and_ (le x y) (le y x))

(* Conditional *)
let ite b x y = if_zero b y x

(* Lists of numbers represented as repeated pairing *)
let empty = one
(* a pair ignores its first argument, empty (= one) does not *)
let is_empty (Ch s) = lt (s (fun x -> x) zero) (s add1 zero)
let head = fst
let tail = snd

(* Recursion!                                                       *)
(* This uses a recursively typed version of the fixpoint combinator *)
type 'a fix = Fix of ('a fix -> 'a)

(* The usual fixpoint combinator Y doesn't work with CBV *)
(* let y t =                           *)
(*   let t' (Fix f) = t (f (Fix f)) in *)
(*   t' (Fix t')                       *)

(* A version of Y that works with CBV *)
let y t =
  let t' (Fix f) = t (fun z -> f (Fix f) z) in
  t' (Fix t')
  
(* The 'then' and 'else' clauses are wrapped in thunks *)
(* to simulate nonstrict if-then-else                  *)
let fact =
  let t_fact f x =
    apply (if_zero x
          (Ch (fun _ _ -> one))
          (Ch (fun _ _ -> (mul x (f (sub1 x))))))
          (fun x -> x) zero in
    y t_fact

let fib =
  let t_fib f x =
    apply (ite (lt x two)
          (Ch (fun _ _ -> x))
          (Ch (fun _ _ -> (add (f (sub1 x)) (f (sub x two))))))
          (fun x -> x) zero in
    y t_fib
    
let length =
  let t_length f x =
    apply (ite (is_empty x)
          (Ch (fun _ _ -> zero))
          (Ch (fun _ _ -> (add1 (f (snd x))))))
          (fun x -> x) zero in
    y t_length

(* Conversions Church -> int and int -> Church                *)
(* for convenience only -- these are not part of the encoding *)
let rec to_church n =
  if n = 0 then zero
  else add1 (to_church (n - 1))

let rec from_church (Ch n) =
  try ignore (n (fun _ -> failwith "") zero); 0
  with _ -> 1 + from_church (sub1 (Ch n))

(* Examples:                                               *)
(* # from_church (add six seven);;                         *)
(* - : int = 13                                            *)
(* # from_church (mul six seven);;                         *)
(* - : int = 42                                            *)
(* # let list = (pair one (pair two (pair three empty)));; *)
(* val list : Church.church = Ch <fun>                     *)
(* # from_church (length list);;                           *)
(* - : int = 3                                             *)
(* # from_church (head (tail (tail list)));;               *)
(* - : int = 3                                             *)
(* # from_church (fact four);;                             *)
(* - : int = 24                                            *)
(* # from_church (fib ten);;                               *)
(* - : int = 55                                            *)