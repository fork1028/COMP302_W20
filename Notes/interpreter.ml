(* This is the real interpreter.  It matches the examples. *)

(* OCaml code for an interpreter for a simple lambda-calculus based language.
** The evaluation strategy is call by value.  Substitution is handled by an
** environment mechanism.  Static binding is enforced by using closures.
** The evaluator takes terms of the datatype "lterm" and returns results of
** type "results".  Plus is provided as a primitive operation.  The
** environment is structured as a layered list, each layer representing a new
** scope.  Each layer is essentially a list of (variable, result) pairs.
** The types "results", "layer" and "envs" are defined by mutual
** recursion.  A Let expression has a list of (variable, expression) pairs
** to represent the new declarations, and also a body as its second component.
*)

type lterm = 
  |  Num of int 
  |  Var of string  
  |  Apply of lterm *lterm 
  |  Plus of lterm *lterm 
  |  Lambda of string * lterm 
  |  Let of (( string * lterm) list) *lterm;;

type results  = 
  | Int of int  
  | Closure of (lterm * envs)
  | Unbound
and 
  layer = Layer of (lterm * results) list
and
  envs = Env of layer list;;

(* The following exceptions are used to catch improper arguments.  The
** exception handling mechanism is not being used.  This just illustrates the
** definition of exceptions and their use in programs.  
*) 

(* The following functions are utilities to search through and to build up
** environments and closures.
*)

let first_part (x,y) = x;;

let sec_part (x,y) = y;;

let toplayer env =
  match env with
    | Env (top :: rest) -> top
    | _ -> failwith "Not a proper environment";;

let firstpair layer =
  match layer with
    | Layer (first:: rest) -> first
    | _ -> failwith "Improper layer";;     

(* The next function searches for a variable in a layer.  Search_layer
** exists only to strip off the constructor and then call bound_in which
** searches a list.  Search_env and binding interact analogously.
*)

let rec bound_in (layer, var) =
  match var with
    | Var name -> 
        (match layer with
          | [] ->  (false, Unbound) 
          | (x,y)::rest -> 
            if x = (Var name) then (true, y)
            else bound_in (rest,var) )
    | _ -> failwith "Searching for a non-variable";;

let search_layer (layer, var) = 
  match layer with
    | Layer l -> bound_in(l, var)
    | _ -> failwith "Searching in a bad layer.";;

let rec binding (bare_env, var) =
  match var with
    | Var name -> 
        (match bare_env with
          | []  -> Unbound
          | x :: rest -> 
               let (p,q) = search_layer(x, var) in
               if p then q else binding(rest, var) )
    | _ -> failwith "Searching for a non-variable";;

let newenv (x, Env n) =  Env ((Layer x)::n);;

let search_env (Env e, Var x) = binding (e, Var x);;

let is_closure arg =
  match arg with
    | Closure _ -> true 
    | _ -> false;;

(* The following functions build closures and take them apart. *)

let lambda_body (Lambda (x,y)) = y;; 

let boundvar (Lambda (x,y))   = x;;

let get_body (Closure (exp, env))= lambda_body exp;;

let get_var (Closure (exp, env))= Var (boundvar exp);;

let get_env (Closure (exp, env)) = env;;

let make_closure (Lambda (n,exp), Env e) = Closure (Lambda (n,exp), Env  e);;

(* The following implements the primitive arithmetic operation in terms of
built-in arithmetic. *)

let myadd (Int x, Int  y) = Int (x+y);;

(* The following three functions are defined mutually recursively.  
** Make_new_layer makes a new layer from a list of (variable, expression)
** declarations.  It needs to call the evaluator to evaluate the expressions
** so that the layer being constructed has (variable, result) pairs in it.  
** Apply expects a closure and an evaluated argument.  It recursively calls
** eval on the body once the new binding has been established.
*) 

let rec make_new_layer (list_of_pairs, env) =
  match list_of_pairs with
    | []  -> []
    | (name, expr)::rest -> 
        (Var name, eval(expr, env))::make_new_layer(rest, env) 
and apply (closure, arg) = eval((get_body(closure)),
                                (newenv([((get_var closure), arg)],
                                        (get_env  closure))))
and eval (term, env) = 
  match term with
    | Var name ->  search_env (env, Var name)
    | Num y ->  Int y 
    | Plus (exp1,exp2) ->  myadd(eval (exp1,env),
                                 eval(exp2,env)) 
    | Lambda (param, body) -> make_closure (Lambda (param, body) , env)
    | Apply (func, arg) -> apply(eval(func, env), eval(arg, env))
    | Let (binding_list, let_body) ->
        eval (let_body, 
              newenv((make_new_layer(binding_list, env),env)));;



(* Examples below *)

let p = Let ( [("b", Num 2);("d",Num 4)], Apply(Lambda ("c", Plus(Plus(Var "c", Var "b"), Var "d")), Num 3));; 

let q = Let ( [("b", Num 2)], Apply(Lambda ("c",Plus(Var "c", Var
"b")),Num 3));; 

let x = Lambda ("y",Plus (Var "y", Var "y"));;

let z = Apply (x, Num 2);;

let l1 = [(Var "a",Int 2);(Var "b",Int 3)];;

let l2 = [(Var "c", Int 3);(Var "d", Int 4)];;

let e1 =Env ( [Layer l1;Layer l2]);;


let d = Apply (Lambda ("e",Plus (p, Var "e")), Num 5);;

let e = Let ([("f",q)], Apply (Lambda("s", Plus(Plus(Var "s",q), 
                                                       Var "f")), Num 3));; 

(* This returns a function as a closure. *)
let f = Apply(Lambda ("x", Var "x"), Lambda ("u", Var "u"));;

let k1 = Let ([("x", Num 1)], Let ([("f", Lambda ("u", Plus(Var "u", Var "x")))], Let ([("x", Num 2)], Apply(Var "f", Var "x"))));;

let k2 = Apply (Lambda ("x", Var "x"), (Lambda ("y", Plus (Var "y", Num 1))));;

(* Use this with e1 *)
let k3 = Apply (Lambda ("x", Plus (Var "x", Var "a")), Var "c");;

let fresh = Env [ (Layer []) ]

let d1 = Apply(Lambda ("f", Lambda ("x", Apply (Var "f", Var "x"))),
               Lambda ("n", Plus(Var "n", Num 1)))

let d2 = Apply(d1, Num 3)
