(* Question 1 *)

let makeProtectedAccount((openingBalance: int), (password: string)) =
    let isOpen = ref true in 
    let balance = ref openingBalance in 
    let passwd = ref password in 
    fun ((passkey:string), (t : transaction)) ->
      if not !isOpen 
      then 
        print_string "Account closed."
      else
        if not (passkey = !passwd) then
          print_string "Incorrect password."
        else
          match t with
          | Withdraw(x) ->
             if (x < !balance)
             then
               (balance := !balance - x;
               Printf.printf "The new balance is: %i." !balance)
            else
              print_string "Insufficient funds."
          | Deposit(x) ->
              balance := !balance + x;
              Printf.printf "The new balance is: %i." !balance
          | CheckBalance ->
              Printf.printf  "The balance is: %i." !balance
          | ChangePassword newPwd -> (passwd := newPwd; print_string "Password changed.")
          | Close -> isOpen := false; print_string "Account successfully closed.";;
