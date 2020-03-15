(* Q1 *)

type term = Term of float * int
type poly = Poly of (float * int) list

exception EmptyList

let multiplyPolyByTerm (Term(c,e), Poly p) =
  if p = [] then raise EmptyList
  else
    if (c = 0.0) then
      Poly [(0.0,0)]
    else
      Poly (List.map (fun ((a,d): float * int) -> (a *. c,e+d)) p)

(* Q1 *)
           
let p1:poly = Poly [(3.0,5);(2.0,2);(7.0,1);(1.5,0)]
let t1: term = Term (2.0,2)

(* Use a recursive helper function inside. *)
                    
let addTermToPoly(Term(c,e), Poly p) =
  let rec helper q = 
    match q with
    | [] -> [(c,e)]
    | (a,d)::ptail ->
              if (e > d) then (c,e)::q
              else
                if (e = d) then
                  if (a +. c) = 0.0 then ptail
                  else (a +. c,d) :: ptail
                else
                  (a,d)::(helper(ptail))
  in                       
  match p with
    | [] -> raise EmptyList
    | _ ->
       let tmp = helper p
       in
       if tmp = [] then Poly [(0.0,0)] else Poly tmp

let addPolys(Poly p1, Poly p2) =
  let rec helper(Poly q1, Poly q2) =
    match q1 with
    | [] -> Poly q2
    | (c,e)::q1tail -> helper(Poly q1tail, addTermToPoly(Term(c,e), Poly q2))
  in
  if ((p1 = [])  || (p2 = [])) then raise EmptyList
  else
    let tmp = helper(Poly p1, Poly p2)
    in
    match tmp with
    | Poly l ->
        if l = [] then Poly [(0.0,0)] else tmp

let multPolys(Poly p1, Poly p2) =
  let rec helper(Poly q1, Poly q2) = 
    match q1 with
    | [] -> raise EmptyList
    | [(c,e)] -> multiplyPolyByTerm(Term (c,e), Poly q2)
    | (c,e)::q1tail -> addPolys(multiplyPolyByTerm(Term (c,e), Poly q2),helper(Poly q1tail, Poly q2))
  in
  if ((p1 = []) || (p2 = [])) then raise EmptyList
  else
    if ((p1 = [(0.0,0)]) || (p2 = [(0.0,0)])) then Poly [(0.0,0)]
    else helper(Poly p1, Poly p2);;

           
(* Q2 *)

type cell = { data : int; next : rlist}
and rlist = cell option ref;;

let c1 = {data = 1; next = ref None};;
let c2 = {data = 2; next = ref (Some c1)};;
let c3 = {data = 3; next = ref (Some c2)};;
let c5 = {data = 5; next = ref (Some c3)};;

(* This converts an rlist to an ordinary list. *)
let rec displayList (c : rlist) =
  match !c with
    | None -> []
    | Some { data = d; next = l } -> d :: (displayList l);;

let cell2rlist (c:cell):rlist = ref (Some c);;
    
let bigger((x:int), (y:int)) = (x > y);;

let rec insert comp (item: int) (list: rlist) =
  match !list with
    | None -> 
        list := Some { data = item ; next = ref None}
    | Some { data = d } when comp (item, d) ->
        let newCell = Some { data = item; next = ref (!list) } in list := newCell
    | Some { next = tail } ->
        insert comp item tail;;
