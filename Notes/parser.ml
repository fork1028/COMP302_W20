let example = "(a+(b+(c*d)+e)*f)*g";;
let example2 = "(a+b)*c";;
let example3 = "((a))+b";;
let example4 = "a+b+c+d";;
let example5 = "a+(b+(c+d))";;
let example6 = "((a+b)*(c+d)+e+f*g+h)";;
let badex1 = "a++b";;
let badex2 = "a+(b+c)*2";;
let badex3 = "a+)";;
let badex4 = "a+(b+";;
(*
# parse example;;
- : exptree =
Expr ('*',
 Expr ('+', Var 'a',
  Expr ('*',
   Expr ('+', Var 'b', Expr ('+', Expr ('*', Var 'c', Var 'd'), Var 'e')),
   Var 'f')),
 Var 'g')
# parse example2;;
- : exptree = Expr ('*', Expr ('+', Var 'a', Var 'b'), Var 'c')
# parse example3;;
- : exptree = Expr ('+', Var 'a', Var 'b')
# parse example4;;
- : exptree =
Expr ('+', Var 'a', Expr ('+', Var 'b', Expr ('+', Var 'c', Var 'd')))
# parse example5;;
- : exptree =
Expr ('+', Var 'a', Expr ('+', Var 'b', Expr ('+', Var 'c', Var 'd')))
# parse example6;;
- : exptree =
Expr ('+',
 Expr ('*', Expr ('+', Var 'a', Var 'b'), Expr ('+', Var 'c', Var 'd')),
 Expr ('+', Var 'e', Expr ('+', Expr ('*', Var 'f', Var 'g'), Var 'h')))
 
# let badex1 = "a++b";;
val badex1 : string = "a++b"
# parse badex1;;
Exception: Failure "In primary".
# let badex2 = "a+(b+c)*2";;
val badex2 : string = "a+(b+c)*2"
# parse badex2;;
Exception: Failure "In primary".
# let badex3 = "a+)";;
val badex3 : string = "a+)"
# parse badex3;;
Exception: Failure "In primary".
# let badex4 = "a+(b+";;
val badex4 : string = "a+(b+"
# parse badex4;;
Exception: Invalid_argument "index out of bounds".
# parse "a + b";;
- : exptree = Var 'a'
*


(* another example *)

type exptree = Var of char | Expr of char * exptree * exptree;;

let charSet = 
 let rec aux i acc =
  if i < 97 then acc else aux (i-1) ((Char.chr i)::acc)
 in aux 122 [];;

let isin (x: char) lst = List.exists (fun y -> x = y) lst;;

let parse (inputexp: string): exptree = 
  let sym = ref inputexp.[0] in
  let cursor = ref 0 in 

  let getsym () =
    (cursor := !cursor + 1);
    (sym := inputexp.[!cursor])
  in

  let rec expr (): exptree =
    let result = term () in
    if !sym = '+' then
      Expr('+',result, (getsym();expr()))
    else
      result
  and term (): exptree =
    let result = primary() in
    if !sym = '*' then
      Expr('*',result, (getsym();term()))
    else
      result
  and primary (): exptree = 
    if !sym = '('
    then
      begin
        getsym();
        let result = expr () in
        if not (!sym = ')') then 
          failwith "Mismatched parens"
        else 
          if (!cursor = String.length(inputexp) - 1) 
          then 
            result
          else 
            ((getsym ()); result)
      end
    else
      if (isin !sym charSet)
      then 
        if (!cursor = String.length(inputexp) - 1) 
        then 
          (Var !sym) 
        else 
          let result = Var !sym in (getsym(); result)
      else
        failwith "In primary"
        (* Printf.printf "sym is : %c." !sym *)
  in
  expr();;

let example = "(a+(b+(c*d)+e)*f)*g";;
let example2 = "(a+b)*c";;
let example3 = "((a))+b";;
let example4 = "a+b+c+d";;
let example5 = "a+(b+(c+d))";;
let example6 = "((a+b)*(c+d)+e+f*g+h)";;
let badex1 = "a++b";;
let badex2 = "a+(b+c)*2";;
let badex3 = "a+)";;
let badex4 = "a+(b+";;
(*
# parse example;;
- : exptree =
Expr ('*',
 Expr ('+', Var 'a',
  Expr ('*',
   Expr ('+', Var 'b', Expr ('+', Expr ('*', Var 'c', Var 'd'), Var 'e')),
   Var 'f')),
 Var 'g')
# parse example2;;
- : exptree = Expr ('*', Expr ('+', Var 'a', Var 'b'), Var 'c')
# parse example3;;
- : exptree = Expr ('+', Var 'a', Var 'b')
# parse example4;;
- : exptree =
Expr ('+', Var 'a', Expr ('+', Var 'b', Expr ('+', Var 'c', Var 'd')))
# parse example5;;
- : exptree =
Expr ('+', Var 'a', Expr ('+', Var 'b', Expr ('+', Var 'c', Var 'd')))
# parse example6;;
- : exptree =
Expr ('+',
 Expr ('*', Expr ('+', Var 'a', Var 'b'), Expr ('+', Var 'c', Var 'd')),
 Expr ('+', Var 'e', Expr ('+', Expr ('*', Var 'f', Var 'g'), Var 'h')))

# let badex1 = "a++b";;
val badex1 : string = "a++b"
# parse badex1;;
Exception: Failure "In primary".
# let badex2 = "a+(b+c)*2";;
val badex2 : string = "a+(b+c)*2"
# parse badex2;;
Exception: Failure "In primary".
# let badex3 = "a+)";;
val badex3 : string = "a+)"
# parse badex3;;
Exception: Failure "In primary".
# let badex4 = "a+(b+";;
val badex4 : string = "a+(b+"
# parse badex4;;
Exception: Invalid_argument "index out of bounds".
# parse "a + b";;
- : exptree = Var 'a'

 *))
