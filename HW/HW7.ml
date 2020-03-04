(* loading the prelude...*)
exception NotImplemented
type typExp =
    TypInt
  | TypVar of char
  | Arrow of typExp * typExp
  | Lst of typExp
type substitution = (char * typExp) list
val te1 : typExp = Arrow (TypInt, Arrow (TypVar 'c', TypVar 'a'))
val te3 : typExp = Arrow (TypVar 'a', Arrow (TypVar 'b', TypVar 'c'))

(* Question 1.1 *)
let rec occurCheck (v: char) (tau: typExp) : bool =
  match tau with
  | TypInt -> false
  | TypVar exp -> v==exp
  | Arrow(tau1,tau2)->occurCheck v tau1 || occurCheck v tau2
  | Lst exp -> occurCheck v exp
;;

(* Question 1.2 *)
let rec substitute (tau1 : typExp) (v : char) (tau2 : typExp) : typExp =
  match tau2 with
  | TypInt -> TypInt
  | TypVar char -> if occurCheck v tau2 then tau1 else TypVar char
  | Arrow(exp1,exp2) -> Arrow((substitute tau1 v exp1),(substitute tau1 v exp2))
  | Lst exp -> Lst (substitute tau1 v exp)
;;

(* Question 1.3 *)
let applySubst (sigma: substitution) (tau: typExp) : typExp =
  List.fold_right (fun (char,exp)-> substitute exp char) sigma tau
;;

(* Question 2 *)
let rec unify (tau1: typExp) (tau2:typExp) : substitution = 
  match (tau1, tau2) with
  | (TypInt, TypInt) -> []
  | (TypVar x, exp) | (exp, TypVar x)-> 
      if TypVar x=exp then [] 
      else if occurCheck x exp then failwith "not unifiable"
      else [(x,exp)] 
  | (Arrow(f,sc),Arrow(g,tc))-> 
      let x=(unify f g) in
      let y=applySubst x sc in
      let z=applySubst x tc in
      (unify y z)@x 
  | ((Lst l1),(Lst l2)) -> unify l1 l2 
  | _ -> failwith "not unifiable"
           
;;
