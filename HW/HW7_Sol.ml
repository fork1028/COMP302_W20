type typExp =
  | TypInt
  | TypVar of char
  | Arrow of typExp * typExp
  | Lst of typExp;;

type substitution = (char * typExp) list;;

(* check if a variable occurs in a term *)
let rec occurCheck (v: char) (tau: typExp) : bool =
  match tau with
    | TypInt -> false
    | TypVar c -> v = c
    | Arrow (tau1,tau2) -> (occurCheck v tau1) || (occurCheck v tau2)
    | Lst tau1 -> occurCheck v tau1;;

(* substitute typExp tau1 for all occurrences of type variable v in typExp tau2 *)
let rec substitute (tau1 : typExp) (v : char) (tau2 : typExp) : typExp =
  match tau2 with
  | TypVar y -> if v = y then tau1 else tau2
  | Arrow(tau3,tau4) -> Arrow ((substitute tau1 v tau3),(substitute tau1 v tau4))
  | Lst tau3 -> Lst (substitute tau1 v tau3)
  | _ -> tau2;;

let applySubst (sigma: substitution) (tau: typExp) : typExp =
  List.fold_right (fun (v, t) -> substitute t v) sigma tau;;


(* May perhaps be correct! *)
let rec unify (tau1: typExp) (tau2:typExp) : substitution = 
  match (tau1,tau2) with
    | (TypInt,TypInt) -> []
    | (TypVar x, TypVar y) -> if (x = y) then [] else [(x, tau2)]
    | (TypVar x, _) -> if not (occurCheck x tau2)
                       then
                         [(x, tau2)]
                       else failwith "Failed occurs check" 
    | (_, TypVar y) -> if not (occurCheck y tau1)
                       then
                         [(y, tau1)]
                       else failwith "Failed occurs check"
    | (TypInt, _) -> failwith "Not unifiable"
    | (_, TypInt) -> failwith "Not unifiable"
    | (Lst tau3, Lst tau4) -> unify tau3 tau4
    | (Arrow(tau3,tau4),Arrow(tau5,tau6)) -> 
         let sigma1 = (unify tau3 tau5) in
         let tau4new = applySubst sigma1 tau4 in
         let tau6new = applySubst sigma1 tau6 in    
         let sigma2 = unify tau4new tau6new in
         sigma2 @ sigma1
    | (_,_) -> failwith "Clash in principal type constructor";;


    

(*   
let te3 = Arrow(TypVar 'a',Arrow(TypVar 'b', TypVar 'c'));;
let te4 = Arrow(Arrow(TypVar 'd', TypVar 'e'), Arrow(TypVar 'f', TypVar 'g'));;
  
let te1 = Arrow(TypInt, Arrow(TypVar 'c', TypVar 'a'));;
val te1 : typExp = Arrow (TypInt, Arrow (TypVar 'c', TypVar 'a'))
# let te3 = Arrow(TypVar 'a',Arrow (TypVar 'b',TypVar 'c'));;
val te3 : typExp = Arrow (TypVar 'a', Arrow (TypVar 'b', TypVar 'c'))
# let result = unify te1 te3;;
val result : substitution = [('b', TypInt); ('c', TypVar 'b'); ('a', TypInt)]
# applySubst result te1;;
- : typExp = Arrow (TypInt, Arrow (TypInt, TypInt))
# applySubst result te3;;
- : typExp = Arrow (TypInt, Arrow (TypInt, TypInt))
 *)
