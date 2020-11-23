#use "reader.ml";;
open Reader;;

type constant =
  | Sexpr of sexpr
  | Void

type expr =
  | Const of constant
  | Var of string
  | If of expr * expr * expr
  | Seq of expr list
  | Set of expr * expr
  | Def of expr * expr
  | Or of expr list
  | LambdaSimple of string list * expr
  | LambdaOpt of string list * string * expr
  | Applic of expr * (expr list);;

let rec expr_eq e1 e2 =
  match e1, e2 with
  | Const Void, Const Void -> true
  | Const(Sexpr s1), Const(Sexpr s2) -> sexpr_eq s1 s2
  | Var(v1), Var(v2) -> String.equal v1 v2
  | If(t1, th1, el1), If(t2, th2, el2) -> (expr_eq t1 t2) &&
                                            (expr_eq th1 th2) &&
                                              (expr_eq el1 el2)
  | (Seq(l1), Seq(l2)
    | Or(l1), Or(l2)) -> List.for_all2 expr_eq l1 l2
  | (Set(var1, val1), Set(var2, val2)
    | Def(var1, val1), Def(var2, val2)) -> (expr_eq var1 var2) &&
                                             (expr_eq val1 val2)
  | LambdaSimple(vars1, body1), LambdaSimple(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr_eq body1 body2)
  | LambdaOpt(vars1, var1, body1), LambdaOpt(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr_eq body1 body2)
  | Applic(e1, args1), Applic(e2, args2) ->
     (expr_eq e1 e2) &&
       (List.for_all2 expr_eq args1 args2)
  | _ -> false;;
	
                       
exception X_syntax_error;;

module type TAG_PARSER = sig
  val tag_parse_expressions : sexpr list -> expr list
  
  val reserved_word_list : string list 
  val const_sexpr : sexpr -> expr 
  val const_expr_not_standart : sexpr * sexpr -> expr
  val var_expr : sexpr -> expr 


end;; (* signature TAG_PARSER *)

module Tag_Parser : TAG_PARSER = struct

let reserved_word_list =
  ["and"; "begin"; "cond"; "define"; "else";
   "if"; "lambda"; "let"; "let*"; "letrec"; "or";
   "quasiquote"; "quote"; "set!"; "pset!"; "unquote";
   "unquote-splicing"];;  

(* work on the tag parser starts here *)

 
(* read_sexprs "(define lambda 3) (define x 6)";;
- : sexpr list =      [Pair (Symbol "define", Pair (Symbol "lambda", Pair (Number (Fraction (3, 1)), Nil)));                
                        Pair (Symbol "define", Pair (Symbol "x", Pair (Number (Fraction (6, 1)), Nil)))]


read_sexprs "(define y '(1 2))";;
- : sexpr list =    [Pair (Symbol "define",  
                                  Pair (Symbol "y",  Pair (Pair (Symbol "quote", Pair (Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Nil)), Nil)), Nil)))] *)



 (* (Booleans, chars, numbers, strings) *)

let const_sexpr x = match x with
  | Bool(s) -> Const(Sexpr(x))
  | Number(s) -> Const(Sexpr(x))
  | Char(s) -> Const(Sexpr(x))
  | String(s) -> Const(Sexpr(x))
  | _ -> raise X_no_match;;
                  
let const_expr_not_standart (car,cdr) = match car with
  | Symbol("quote") -> Const(Sexpr(cdr))
  | _ -> const_sexpr car;;


let rec expr (car,cdr) = match car with
  | Symbol("if") -> if_expr cdr
  | Symbol(s) -> var_expr cdr
  | _ -> const_expr_not_standart (car,cdr)

  
(* TODO CHECK THAT WE ARE NOT RESERVED LIST *)
and let var_expr x = match x with 
  | Symbol(s) -> Var(s) 
  | _ -> raise X_no_match



and let if_expr (tes, (the, els))= pack x ()
  let test_exp = expr tes in
  let then_exp = expr the in
  let else_exp = expr els in
  If(test_exp, then_exp, else_exp );;

(* let define_expr = pack x (fun Pair(car1, cdr1) -> Define());; *)

(* 3. Conditionals - supprot if-then & if-then-else ==> when we have if-then ==> we sould convert it to if-then-else where the else is Const(VOID) *)
(* Disjunctions are simply or-expressions *)

read_sexprs "(if #t 1 2)";;
- : sexpr list =   
[Pair (Symbol "if",                                                               
  Pair (Bool true,                                                              
   Pair (Number (Fraction (1, 1)), 
   Pair (Number (Fraction (2, 1)), Nil))))]

read_sexprs "(if #t 1)";;
- : sexpr list =         
[Pair (Symbol "if", 
  Pair (Bool true, 
  Pair (Number (Fraction (1, 1)), 
        Nil)))]  

(if (> 4 5) (+ 4 5) (- 4 5))


let tag_parse_expressions sexpr = raise X_not_yet_implemented;;


  
end;; (* struct Tag_Parser *)

