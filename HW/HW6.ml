(* loading the prelude...*)
exception NotImplemented
type transaction =
    Withdraw of int
  | Deposit of int
  | CheckBalance
  | ChangePassword of string
  | Close


(* Question 1 *)

let makeProtectedAccount ((openingBalance: int), (password: string)) =
  let isClosed = ref false in
  let balance = ref openingBalance in 
  let key = ref password in
  fun ((pwd : string), (t: transaction)) ->
    if !isClosed = false then
      if pwd = !key then
        match t with
        | Withdraw(m) ->  if (!balance >= m)
            then
              ((balance := !balance - m);
               (Printf.printf "The new balance is: %i." !balance))
            else
              print_string "Insufficient funds."
        | Deposit(m) ->
            ((balance := !balance + m);
             (Printf.printf "The new balance is: %i." !balance)) 
        | CheckBalance -> (Printf.printf "The balance is: %i." !balance)
        | ChangePassword(str) -> key := str; Printf.printf "Password changed." 
        | Close -> isClosed := true; Printf.printf "Account successfully closed."
      else Printf.printf "Incorrect password."      
    else Printf.printf "Account closed."
                          
;;
