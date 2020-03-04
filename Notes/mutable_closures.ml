let a = ref 0;;
let flip0 () = (a := (1 - !a)); (Printf.printf "%i\n" !a);;

let flip =
  let c = ref 0 in
  fun () -> (c := 1 - !c); (Printf.printf "%i\n" !c);;

let flip2 =
  let c = ref 0 in
  fun () -> (c := 1 - !c); (Printf.printf "%i\n" !c);;

let make_flipper = fun () ->
  let c = ref 0 in
  fun () -> (c := 1 - !c); (Printf.printf "%i\n" !c);;

(* Datatype of bank account transactions.*)
type transaction = Withdraw of int | Deposit of int | Checkbalance

(* Bank account generator. *)
let make_account(opening_balance: int) =
    let balance = ref opening_balance in
    fun (t: transaction) ->
      match t with
        | Withdraw(m) ->  if (!balance > m)
                          then
                            ((balance := !balance - m);
                            (Printf.printf "Balance is %i" !balance))
                          else
                            print_string "Insufficient funds."
        | Deposit(m) ->
           ((balance := !balance + m);
            (Printf.printf "Balance is %i\n" !balance)) 
        | Checkbalance -> (Printf.printf "Balance is %i\n" !balance);;


(* Monitoring *)
type 'a tagged = Query | Normal of 'a
type 'b answers = Numcalls of int | Ans of 'b

let makeMonitored f =
    let c = ref 0
    fun x ->
      match x with
        | Query -> (Numcalls !c)
        | (Normal y) -> ( c := !c + 1; (Ans (f y)));;

(* To avoid the value restriction we have to use a non-polymorphic version. *)
let monLength = makeMonitored (fun (l : int list) -> (List.length l));;

let inc n = n + 1

let moninc = makeMonitored(inc)
