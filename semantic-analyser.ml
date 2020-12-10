#use "tag-parser.ml";;

type var = 
  | VarFree of string
  | VarParam of string * int
  | VarBound of string * int * int;;

type expr' =
  | Const' of constant
  | Var' of var
  | Box' of var
  | BoxGet' of var
  | BoxSet' of var * expr'
  | If' of expr' * expr' * expr'
  | Seq' of expr' list
  | Set' of var * expr'
  | Def' of var * expr'
  | Or' of expr' list
  | LambdaSimple' of string list * expr'
  | LambdaOpt' of string list * string * expr'
  | Applic' of expr' * (expr' list)
  | ApplicTP' of expr' * (expr' list);;

let rec expr'_eq e1 e2 =
  match e1, e2 with
  | Const' Void, Const' Void -> true
  | Const'(Sexpr s1), Const'(Sexpr s2) -> sexpr_eq s1 s2
  | Var'(VarFree v1), Var'(VarFree v2) -> String.equal v1 v2
  | Var'(VarParam (v1,mn1)), Var'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | Var'(VarBound (v1,mj1,mn1)), Var'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | If'(t1, th1, el1), If'(t2, th2, el2) -> (expr'_eq t1 t2) &&
                                            (expr'_eq th1 th2) &&
                                              (expr'_eq el1 el2)
  | (Seq'(l1), Seq'(l2)
  | Or'(l1), Or'(l2)) -> List.for_all2 expr'_eq l1 l2
  | (Set'(var1, val1), Set'(var2, val2)
  | Def'(var1, val1), Def'(var2, val2)) -> (expr'_eq (Var'(var1)) (Var'(var2))) &&
                                             (expr'_eq val1 val2)
  | LambdaSimple'(vars1, body1), LambdaSimple'(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr'_eq body1 body2)
  | LambdaOpt'(vars1, var1, body1), LambdaOpt'(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr'_eq body1 body2)
  | Applic'(e1, args1), Applic'(e2, args2)
  | ApplicTP'(e1, args1), ApplicTP'(e2, args2) ->
	 (expr'_eq e1 e2) &&
	   (List.for_all2 expr'_eq args1 args2)
  | _ -> false;;
	
                       
exception X_syntax_error;;
exception X_entering_lambda;;
exception X_check_Lexical;;
exception X_annotate_lexical_addresses;;



module type SEMANTICS = sig
  
  val entering_lambda : var list -> var list 
  val param_list_to_var_params : string list -> var list 
  val check_Lexical : var list * string -> var

  
  val run_semantics : expr -> expr'
  val annotate_lexical_addresses : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val box_set : expr' -> expr'




end;;

module Semantics : SEMANTICS = struct


(* let rec index_of lst index exp = match exp with
  | (List.hd lst) -> index
  | _ -> index_of (List.tl lst) (index + 1) exp *)


  
(* entering lambda changes all the var_params to var_bounds and make +1 to the envs *)

let entering_lambda lst = 
  let add_one_to_majors_env var = match var with
  | VarParam(var_name,minor) -> VarBound(var_name,0,minor)
  | VarBound(var_name,major,minor) ->VarBound(var_name,major + 1,minor)
  | _ -> raise X_entering_lambda
  in
    List.map add_one_to_majors_env lst;;


(* from string_name list to var_params *)
let param_list_to_var_params param_list = 
  let rec param_list_to_var_params_rec param_list index var_Params  =
  if(List.length param_list == 0) 
  then var_Params
  else param_list_to_var_params_rec (List.tl param_list) (index + 1) var_Params@[VarParam((List.hd param_list), index)]
  in
  param_list_to_var_params_rec param_list 0 [];;

  
(* for every var we create the correct var type based on the env *)
(* val check_Lexical : var list * string -> var *)

(* let check_Lexical (lst, str_name) = 
  
  let rec find_var lst = match lst with
  | [] -> VarFree(str_name)
  | hd::tl -> check_vars lst
  and check_vars lst = match (List.hd lst) with
    | VarParam(name,minor) -> if (String.equal str_name name) then VarParam(str_name,minor) else (check_vars (List.tl lst))
    | VarBound(name,major,minor) ->if (String.equal str_name name) then VarBound(str_name,major,minor) else (check_vars (List.tl lst))
    | _ -> (find_var (List.tl lst))
  in
  find_var lst;; *)




  let check_Lexical (lst, str_name) = 
  
    let rec find_var lst = match (List.hd lst) with
      | VarParam(name,minor) -> if (String.equal str_name name) then VarParam(str_name,minor) else (check_empty_list (List.tl lst))
      | VarBound(name,major,minor) ->if (String.equal str_name name) then VarBound(str_name,major,minor) else (check_empty_list (List.tl lst))
      | _ -> raise X_check_Lexical

    and check_empty_list lst = match lst with
      | [] -> VarFree(str_name)
      | _ -> find_var lst
    in
    check_empty_list lst;;


  




let annotate_lexical_addresses expr =
  let rec recursive_lexical lst expr = match expr with
    | Const(x) -> Const'(x)
    | Var(x) -> Var'(check_Lexical (lst, x))
    | If(test_expr, then_expr, else_expr)  -> If'(recursive_lexical lst test_expr, recursive_lexical lst then_expr, recursive_lexical lst else_expr)
    | Seq(seq_lst) -> Seq'(List.map (recursive_lexical lst) seq_lst)
    | Set(Var(x),bval) -> Set'((check_Lexical (lst, x)), (recursive_lexical lst bval))
    | Def(Var(x),bval) -> Def'((check_Lexical (lst, x)), (recursive_lexical lst bval))
    | Or(or_lst) -> Or'(List.map (recursive_lexical lst) or_lst)
    | LambdaSimple(param_list, body_exp ) -> LambdaSimple'(param_list,
                                                          (recursive_lexical ((entering_lambda lst)@(param_list_to_var_params param_list)) body_exp))
    | LambdaOpt(param_list, var_variadic ,body_exp ) -> LambdaOpt'(param_list,
                                                                    var_variadic,
                                                                    (recursive_lexical ((entering_lambda lst)@(param_list_to_var_params param_list)) body_exp))
    | Applic(expr, expr_list) -> Applic'(recursive_lexical lst expr , List.map (recursive_lexical lst) expr_list )
    | _ -> raise X_annotate_lexical_addresses
    in
    recursive_lexical [] expr;;


    
let annotate_tail_calls expr_tag = raise X_not_yet_implemented;;

let box_set expr_tag = raise X_not_yet_implemented;;

let run_semantics expr =
  box_set
    (annotate_tail_calls
       (annotate_lexical_addresses expr));;
  
end;; (* struct Semantics *)
open Semantics;;




(* 
 


( (LambdaSimple (["x"; "y"; "z"],
  Seq
   [LambdaSimple (["y"],
     Seq
      [Set (Var "x", Const (Sexpr (Number (Fraction(5,1)))));
       Applic (Var "+", [Var "x"; Var "y"])]);
    Applic (Var "+", [Var "x"; Var "y"; Var "z"])])) )

    
=========================

( (LambdaSimple (["x"],
  Seq
   [LambdaSimple (["x"], Set (Var "x", Var "x"));
    LambdaSimple (["x"], Set (Var "x", Var "x"))])) )
    
    
    
    
    
    (
LambdaSimple' (["x"],
 Seq'
  [LambdaSimple' (["x"],
    Set'( (VarParam ("x", 0)), Var' (VarParam ("x", 0))));
   LambdaSimple' (["x"],
    Set'( (VarParam ("x", 0)), Var' (VarParam ("x", 0))))])) ;;

===================================



=====


    (
LambdaSimple' (["x"; "y"; "z"],
 Seq'
  [
   LambdaSimple' (["y"],
      Seq'
       [Set' (VarBound ("x", 0, 0), Const' (Sexpr (Number (Fraction(5,1)))));
        ApplicTP' (Var' (VarFree "+"),
         [Var' (VarBound ("x", 0, 0)); Var' (VarParam ("y", 0))])]);
     ApplicTP' (Var' (VarFree "+"),
      [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
       Var' (VarParam ("z", 2))])]))







( (LambdaOpt (["x"; "y"], "z",
 Applic (Var "+",
  [Var "x"; Var "y";
   LambdaSimple (["z"],
    Applic (Var "+",
     [Var "z"; LambdaSimple (["z"], Applic (Var "+", [Var "z"; Var "y"]))]))]))) )
     
     
     
     
     
     (
LambdaOpt' (["x"; "y"], "z",
 ApplicTP' (Var' (VarFree "+"),
  [Var' (VarParam ("x", 0)); Var' (VarParam ("y", 1));
   LambdaSimple' (["z"],
    ApplicTP' (Var' (VarFree "+"),
     [Var' (VarParam ("z", 0));
      LambdaSimple' (["z"],
       ApplicTP' (Var' (VarFree "+"),
        [Var' (VarParam ("z", 0)); Var' (VarBound ("y", 1, 1))]))]))])))  *)