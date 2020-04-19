(* Examples of the code generator. *)

(*


# let example = "(a+(b+(c*d)+e)*f)*g";;
val example : string = "(a+(b+(c*d)+e)*f)*g"
# let t1 = parse example;;
val t1 : exptree =
  Expr ('*',
   Expr ('+', Var 'a',
    Expr ('*',
     Expr ('+', Var 'b', Expr ('+', Expr ('*', Var 'c', Var 'd'), Var 'e')),
     Var 'f')),
   Var 'g')
# codegen t1;;
LOAD  a
STORE 1
LOAD  b
STORE 2
LOAD  c
MUL  d
ADD  e
ADD 2
MUL  f
ADD 1
MUL  g
- : unit = ()
# let example6 = "((a+b)*(c+d)+e+f*g+h)";;
val example6 : string = "((a+b)*(c+d)+e+f*g+h)"
# let t6 = parse example6;;
val t6 : exptree =
  Expr ('+',
   Expr ('*', Expr ('+', Var 'a', Var 'b'), Expr ('+', Var 'c', Var 'd')),
   Expr ('+', Var 'e', Expr ('+', Expr ('*', Var 'f', Var 'g'), Var 'h')))
# codegen t6;;
LOAD  a
ADD  b
STORE 1
LOAD  c
ADD  d
MUL 1
STORE 1
LOAD  e
STORE 2
LOAD  f
MUL  g
ADD  h
ADD 2
ADD 1
- : unit = ()

 *)

(* another example *)

let tempstore = ref 0;;
  
type exptree = Var of char | Expr of char * exptree * exptree;;

let codegen (e: exptree) = 
  let rec helper((e: exptree), (tag: char)) =
    match e with
      | Var c ->
        if (tag = '=') then
          Printf.printf "LOAD  %c\n" c
        else
          if (tag = '+')
          then
            Printf.printf "ADD  %c\n" c
          else (* tag = '*' *)
            Printf.printf "MUL  %c\n" c
      | Expr(op,l,r) -> 
         if (tag = '=') then
           (helper (l,'=');
           helper (r, op))
         else
           begin
             tempstore := !tempstore + 1;
             Printf.printf "STORE %i\n" !tempstore;
             helper(l,'=');
             helper(r,op);
             (if (tag = '+')
             then
               Printf.printf "ADD %i\n" !tempstore
             else
               Printf.printf "MUL %i\n" !tempstore);
             tempstore := !tempstore - 1
           end
  in
  helper(e,'=');;

(*
         
let exptree = Expr ('+',Var 'a',Expr ('*',Var 'b',Expr ('+',Var 'c',Var 'd')));;
# codegen exptree;;
LOAD  a
STORE 1
LOAD  b
STORE 2
LOAD  c
ADD  d
MUL 2
ADD 1
- : unit = ()

 *)
