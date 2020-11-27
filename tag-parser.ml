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
  val symbol_extract_fun : string list -> sexpr -> string list
  val tag_pareser : sexpr -> expr

  val tag_parse_expressions : sexpr list -> expr list
  
  (* val reserved_word_list : string list 
  val const_sexpr : sexpr -> expr 
  val const_expr_not_standart : sexpr * sexpr -> expr
  val var_expr : sexpr -> expr  *)


end;; (* signature TAG_PARSER *)

module Tag_Parser : TAG_PARSER = struct

let reserved_word_list =
  ["and"; "begin"; "cond"; "define"; "else";
   "if"; "lambda"; "let"; "let*"; "letrec"; "or";
   "quasiquote"; "quote"; "set!"; "pset!"; "unquote";
   "unquote-splicing"];;  

(* work on the tag parser starts here *)




(* 3. Conditionals - supprot if-then & if-then-else ==> when we have if-then ==> we sould convert it to if-then-else where the else is Const(VOID) *)
(* Disjunctions are simply or-expressions *)

let rec symbol_extract_fun lst sexpr = match sexpr with
  | Nil -> lst
  | Pair(Symbol(s),rest) -> (symbol_extract_fun (lst@[s]) rest)
  | _ -> raise X_no_match;;

  
let rec tag_pareser sexpr = match sexpr with
  | Bool(s) -> Const(Sexpr(sexpr))
  | Number(s) -> Const(Sexpr(sexpr))
  | Char(s) -> Const(Sexpr(sexpr))
  | String(s) -> Const(Sexpr(sexpr))
  | Pair(Symbol "quote", Pair(sexpr, Nil)) -> Const(Sexpr(sexpr))
  | Nil -> Const(Void)

  (* TODO CHECK THAT WE ARE NOT RESERVED LIST *)
  | Symbol(s) -> if (not (List.mem s reserved_word_list)) then Var(s) else raise X_no_match
  
  
  | Pair(Symbol "if", Pair(test_sexp, Pair(then_sexp, Pair(else_sexp, Nil)))) ->
      let test_exp = (tag_pareser test_sexp) in
      let then_exp = (tag_pareser then_sexp) in
      let else_exp = (tag_pareser else_sexp) in
      If(test_exp, then_exp, else_exp)
  
  | Pair(Symbol "if",Pair(test_sexp,Pair(then_sexp ,Nil))) ->
      let test_exp = tag_pareser test_sexp in
      let then_exp = tag_pareser then_sexp in
      If(test_exp, then_exp, Const(Void))
  
  (* | Pair(Symbol "lambda", Pair(Nil, Pair(Pair(Symbol "+", Pair(Number (Fraction(1, 1)), Pair(Number (Fraction(2, 1)), Nil))), Nil))) *)

  | Pair(Symbol "lambda", Pair(params, body)) ->  
        let params_string_list = (symbol_extract_fun [] params) in 
        (* let bodies = (tag_pareser body) in *)
        (* let bodies = (seq_expr body) in *)
        LambdaSimple(params_string_list, Var("============ TODO SEQ ==========="))

  
  (* | Pair(hd, tl) -> seq_expr sexpr *)
        (* let hd_exp = (tag_pareser hd) in 
        let tl_exp = (tag_pareser tl) in
        Seq([hd_exp]@[tl_exp]) *)


  (* | Pair(Symbol "or", Nil) ->  *)
  | Pair(Symbol "or", s) -> 
      let rec expr_list lst sexpr = match sexpr with
      | Nil -> lst
      | Pair(s ,rest) -> (expr_list (lst@[(tag_pareser s)]) rest)
      | _ -> raise X_no_match
      in
      (* if (s==Nil) then Const(Sexpr(Bool(false))) else Or(expr_list [] s) *)
      let conds = match s with
      | Nil -> Const(Sexpr(Bool(false)))
      | Pair(exp ,Nil) -> Const(Sexpr(exp))
      | _ -> Or(expr_list [] s)
      in 
      conds

(*       
      > (print-template '(define b 2))
      Pair(Symbol "define", Pair(Symbol "b", Pair(Number (Fraction(2, 1)), Nil))) *)

  | Pair(Symbol "define", Pair(var, sexpr)) -> 
      let var_exp = tag_pareser var in
      let sexpr_exp = tag_pareser sexpr in
      let def_exp = match var_exp with 
        | Var(s) -> Def(var_exp,sexpr_exp) 
        | _ -> raise X_no_match in
      def_exp

  (* Applic MUST BE THE LAST*)
  | Pair(proc, params) -> 
      let proc_exp = tag_pareser proc in
      let rec params_exp lst sexpr = match sexpr with
        | Nil -> lst
        | Pair(s ,rest) -> (params_exp (lst@[(tag_pareser s)]) rest)
        | _ -> raise X_no_match
      in
      Applic(proc_exp, (params_exp [] params));;

  (* | _ -> raise X_no_match;; *)


(* and seq_expr sexpr = function 
    | Pair(hd, tl) -> 
        let hd_exp = (tag_pareser hd) in 
        let tl_exp = (tag_pareser tl) in
        let seq_extract = match tl_exp with 
        | Seq(x) -> x 
        | _ -> raise X_no_match
        in
        Seq([hd_exp]@[seq_extract])
    | _ -> raise X_no_match ;; *)



    (* (print-template '(+ 4 5) )
    (Pair(Symbol "+", Pair(Number (Fraction(4, 1)), Pair(Number (Fraction(5, 1)), Nil)))) *)
    
    (* > (print-template '((lambda (a) a) 42 ))
    (Pair(Pair(Symbol "lambda", Pair(Pair(Symbol "a", Nil), Pair(Symbol "a", Nil))), Pair(Number (Fraction(42, 1)), Nil))) *)

 
    

    




  (* tag_pareser (Symbol "T");;


  > (print-template '(lambda () #t) )
  Pair(Symbol "lambda", Pair(Nil, Pair(Bool true, Nil)))

  (print-template '(lambda (a b) a) )
  (Pair(Symbol "lambda", Pair(Pair(Symbol "a", Pair(Symbol "b", Nil)),
    Pair(Symbol "a", Nil))))
  


  (print-template '(lambda (n1) n1 #t 42) )
Pair(Symbol "lambda", Pair(
  Pair(Symbol "n1", Nil),
  Pair(Symbol "n1", Pair(Bool true, Pair(Number (Fraction(42, 1)), Nil)))))

(lambda (a b) a b)
 
(Pair(Symbol "lambda", Pair(Pair(Symbol "a", Pair(Symbol "b", Nil)), Pair(Symbol "a", Pair(Symbol "b", Nil)))))



  (print-template '(lambda (a b c d) a))
  Pair(Symbol "lambda", 
                    Pair(
                          Pair(Symbol "a", Pair(Symbol "b", Pair(Symbol "c", Pair(Symbol "d", Nil)))),           
                          Pair(Symbol "a", Nil)
                        ))

(print-template '(lambda () (+ 1 2)))
Pair(Symbol "lambda", Pair(Nil, 
                          Pair(Pair(Symbol "+", Pair(Number (Fraction(1, 1)), Pair(Number (Fraction(2, 1)), Nil))), Nil))) *)


let tag_parse_expressions sexpr = raise X_not_yet_implemented;;



  
end;; (* struct Tag_Parser *)
open Tag_Parser;;

