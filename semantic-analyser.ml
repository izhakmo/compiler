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
    | Box'(VarFree v1), Box'(VarFree v2) -> String.equal v1 v2
    | Box'(VarParam (v1,mn1)), Box'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
    | Box'(VarBound (v1,mj1,mn1)), Box'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
    | BoxGet'(VarFree v1), BoxGet'(VarFree v2) -> String.equal v1 v2
    | BoxGet'(VarParam (v1,mn1)), BoxGet'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
    | BoxGet'(VarBound (v1,mj1,mn1)), BoxGet'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
    | BoxSet'(VarFree v1,e1), BoxSet'(VarFree v2, e2) -> String.equal v1 v2 && (expr'_eq e1 e2)
    | BoxSet'(VarParam (v1,mn1), e1), BoxSet'(VarParam (v2,mn2),e2) -> String.equal v1 v2 && mn1 = mn2 && (expr'_eq e1 e2)
    | BoxSet'(VarBound (v1,mj1,mn1),e1), BoxSet'(VarBound (v2,mj2,mn2),e2) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2 && (expr'_eq e1 e2)
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
exception X_search_var;;
exception X_annotate_lexical_addresses;;
exception X_box_make_the_change_with_box_set_get;;
exception X_box_stuffing_lists;;
exception X_stuffing_lists_stuffing_lists;;
exception X_box_rib_stuffing;;
exception X_;;
exception X_map_stuffing_lists;;


module type SEMANTICS = sig
  
  val entering_lambda : var list -> var list 
  val param_list_to_var_params : string list -> var list 
  val search_var : var list * string -> var
  val list_last_element : 'a list -> 'a
  val list_without_last : 'a list -> 'a list
  val extract_from_3d_array : 'a list list list -> int -> 'a list -> 'a list 
  val box_stuffing_lists : expr' -> string -> expr' list list
  val extract_ribs_3d_array : 'a list list -> 'a list -> 'a list 
  val box_rib_stuffing : expr' -> string -> string list ref list list 



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
  let search_var (lst, str_name) = 
  
    let rec find_var lst = match (List.hd lst) with
      | VarParam(name,minor) -> if (String.equal str_name name) then VarParam(str_name,minor) else (check_empty_list (List.tl lst))
      | VarBound(name,major,minor) ->if (String.equal str_name name) then VarBound(str_name,major,minor) else (check_empty_list (List.tl lst))
      | _ -> raise X_search_var

    and check_empty_list lst = match lst with
      | [] -> VarFree(str_name)
      | _ -> find_var lst
    in
    check_empty_list (List.rev lst);;


  




let annotate_lexical_addresses expr =
  let rec recursive_lexical lst expr = match expr with
    | Const(x) -> Const'(x)
    | Var(x) -> Var'(search_var (lst, x))
    | If(test_expr, then_expr, else_expr)  -> If'(recursive_lexical lst test_expr, recursive_lexical lst then_expr, recursive_lexical lst else_expr)
    | Seq(seq_lst) -> Seq'(List.map (recursive_lexical lst) seq_lst)
    | Set(Var(x),bval) -> Set'((search_var (lst, x)), (recursive_lexical lst bval))
    | Def(Var(x),bval) -> Def'((search_var (lst, x)), (recursive_lexical lst bval))
    | Or(or_lst) -> Or'(List.map (recursive_lexical lst) or_lst)
    | LambdaSimple(param_list, body_exp ) -> LambdaSimple'(param_list,
                                                          (recursive_lexical ((entering_lambda lst)@(param_list_to_var_params param_list)) body_exp))
    | LambdaOpt(param_list, var_variadic ,body_exp ) -> 
                                            let param_strings =  param_list@[var_variadic] in
                                            let param_vars = param_list_to_var_params param_strings in
                                            LambdaOpt'(param_list, var_variadic, (recursive_lexical ((entering_lambda lst)@param_vars) body_exp))
    | Applic(expr, expr_list) -> Applic'(recursive_lexical lst expr , List.map (recursive_lexical lst) expr_list )
    | _ -> raise X_annotate_lexical_addresses
    in
    recursive_lexical [] expr;;

(* 
    Annotate (expr , tp? ) :
    If expr is Var or Const , return expr .
    El seif expr is Applic ,
    If tp? is true , return TC−Applic ( Annotate ( children , #f ) )
    Else return Applic ( Annotate ( children , #f ) )
    Else return expr with its children annotated according to the various rules
 *)

let annotate_tail_calls expr_tag = raise X_not_yet_implemented;;

let list_last_element lst = 
    let rev = (List.rev lst) in
    let last = List.hd rev in
    last;;

let list_without_last lst = 
    let rev = (List.rev lst) in
    let without_last = (List.rev (List.tl rev)) in
    without_last;;

let annotate_tail_calls expr_tag = 
  let rec annotate expr tp = match expr with
  | Const'(x) -> Const'(x)
  | Var'(x) -> Var'(x)
  | If'(test_expr, then_expr, else_expr) -> If'((annotate test_expr false), (annotate then_expr tp), (annotate else_expr tp))
  | Or'(or_lst) ->
          let last = list_last_element or_lst in
          let without_last = list_without_last or_lst in
          let annotate_last = annotate last tp in
          let annotate_map expr = (annotate expr false) in
          let annotate_without_last = (List.map annotate_map without_last) in
              Or'(annotate_without_last@[annotate_last])
  | Def'(x,bval) -> Def'(x, (annotate bval false))
  
  | Seq'(seq_lst) -> 
          let last = list_last_element seq_lst in
          let without_last = list_without_last seq_lst in
          let annotate_last = annotate last tp in
          let annotate_map expr = (annotate expr false) in
          let annotate_without_last = (List.map annotate_map without_last) in
              Seq'(annotate_without_last@[annotate_last])
  
  | Set'(x,bval) -> Set'(x, (annotate bval false))
  | LambdaSimple'(param_list, body_exp ) -> LambdaSimple'(param_list,(annotate body_exp true))
  | LambdaOpt'(param_list, var_variadic ,body_exp ) -> LambdaOpt'(param_list, var_variadic,(annotate body_exp true))
  
  | Applic'(expr, expr_list) -> 
          let annotate_foo expr = (annotate expr false) in
          let annotate_proc = annotate_foo expr in
          let annotate_map = (List.map annotate_foo expr_list) in
                                if (tp == false)
                                then Applic'(annotate_proc, annotate_map)
                                else ApplicTP'(annotate_proc, annotate_map)
  | _ -> raise X_annotate_lexical_addresses in
  (annotate expr_tag false);;






(* box_set_box_get *)
(* list.exist var in shouldbeboxed *)
let box_make_the_change_with_box_set_get expr var_name = 
  let rec boxit expr var_name depth = match expr with
    | Const'(s1)-> Const'(s1)
    | Var'(VarFree v1)-> Var'(VarFree v1)
    | Var'(VarParam (v1,mn1))-> 
                                if((String.equal v1 var_name) && (depth == (-1)))
                                then BoxGet'(VarParam (v1,mn1))
                                else Var'(VarParam (v1,mn1))
    | Var'(VarBound (v1,mj1,mn1))-> 
                                if((String.equal v1 var_name) && (depth == mj1))
                                then BoxGet'(VarBound (v1,mj1,mn1))
                                else Var'(VarBound (v1,mj1,mn1))
    | If'(t1, th1, el1)-> If'(boxit t1 var_name depth, boxit th1 var_name depth, boxit el1 var_name depth)
    | Seq'(l1)->
                            let func expr = boxit expr var_name depth in 
                            let seq_body =  List.map func l1 in
                            Seq'(seq_body)
    | Or'(l1)->
                            let func expr = boxit expr var_name depth in 
                            let or_body =  List.map func l1 in
                            Or'(or_body)
    | Def'(var1, val1)->
                            Def'(var1, boxit val1 var_name depth)
    | Set'(x,bval) ->       let var_type x = match x with
                            | VarParam (v1,mn1) ->
                                                            if ((String.equal v1 var_name) && (depth == (-1)))
                                                            then BoxSet'(x, boxit bval var_name depth)
                                                            else Set'(x, boxit bval var_name depth)
                            | VarBound (v1,mj1,mn1)-> 
                                                            if((String.equal v1 var_name) && (depth == mj1))
                                                            then BoxSet'(x, boxit bval var_name depth)
                                                            else Set'(x, boxit bval var_name depth)
                            | _ -> raise X_box_make_the_change_with_box_set_get
                            in
                            var_type x

    | LambdaSimple'(vars1, body1)->
                            LambdaSimple'(vars1,boxit body1 var_name (depth+1))
                            

    | LambdaOpt'(vars1, var1, body1)->
                            LambdaOpt'(vars1, var1, boxit body1 var_name (depth+1))
    | Applic'(e1, args1)->
                            let func expr = boxit expr var_name depth in
                            let applic_body =  List.map func args1 in
                            Applic'(boxit e1 var_name depth, applic_body)
    | ApplicTP'(e1, args1)->
                            let func expr = boxit expr var_name depth in
                            let applic_body =  List.map func args1 in
                            Applic'(boxit e1 var_name depth, applic_body)
    | _ -> raise X_box_make_the_change_with_box_set_get
    in
    boxit expr var_name (-1);;





(* val extract_from_3d_array : 'a list list list -> int -> 'a list -> 'a list =      <fun>  *)
(* [ [[];[]] ; [[];[]]     ] *)
(* please put 3D arr and index 0 -read or 1 -write ans [] for first result *)
let extract_from_3d_array arr index result = 
  let is_read_write = if (index == 0) then true else false in
  let rec extraction arr index result = match arr with
  | []  -> result
  | _ -> extraction (List.tl arr) index result@(if (is_read_write) then (List.hd (List.hd arr)) else (List.hd (List.tl (List.hd arr))))
  in extraction arr index result;;




  (* we check by the depth and the var_name so we take only the interesting vars that we need *)
let box_stuffing_lists expr var_name =
let rec stuffing_lists expr var_name depth lists  = match expr, lists with
  | Const'(s1), [list_var_read;list_var_write] -> [list_var_read;list_var_write]
  | Var'(VarFree v1), [list_var_read;list_var_write] -> [list_var_read;list_var_write]
  | Var'(VarParam (v1,mn1)), [list_var_read;list_var_write]->
                              if ((String.equal v1 var_name) && (depth == (-1)))
                              then [list_var_read@ [Var'(VarParam (v1,mn1))] ;list_var_write]
                              else [list_var_read;list_var_write]

  | Var'(VarBound (v1,mj1,mn1)), [list_var_read;list_var_write]-> 
                              if((String.equal v1 var_name) && (depth == mj1))
                              then [list_var_read@[Var'(VarBound (v1,mj1,mn1))]; list_var_write]
                              else [list_var_read;list_var_write]
  
  | Set'(x,bval) ,[list_var_read;list_var_write]->

                    let var_type x = match x with
                    | VarParam (v1,mn1) ->
                                                    if ((String.equal v1 var_name) && (depth == (-1)))
                                                    then stuffing_lists bval var_name depth [list_var_read ;list_var_write @[Var'(VarParam (v1,mn1))]]
                                                    else stuffing_lists bval var_name depth [list_var_read;list_var_write]
                    | VarBound (v1,mj1,mn1)-> 
                                                    if((String.equal v1 var_name) && (depth == mj1))
                                                    then stuffing_lists bval var_name depth [list_var_read; list_var_write @[Var'(VarBound (v1,mj1,mn1))]]
                                                    else stuffing_lists bval var_name depth [list_var_read;list_var_write]
                    | _ -> raise X_stuffing_lists_stuffing_lists
                    in
                    var_type x

  | If'(test_exp, then_exp, else_exp), [list_var_read;list_var_write]-> 
                                            let if_lst = [test_exp; then_exp; else_exp] in
                                            map_stuffing_lists if_lst var_name depth [list_var_read;list_var_write]

                                            (* let test_exp = stuffing_lists test_exp var_name depth [[];[]] in
                                             let then_exp = stuffing_lists then_exp var_name depth [[];[]] in
                                             let else_exp = stuffing_lists else_exp var_name depth [[];[]] in
                                             let make_3d_array = [test_exp; then_exp; else_exp] in
                                             let more_var_read = extract_from_3d_array make_3d_array 0 [] in
                                             let more_var_write = extract_from_3d_array make_3d_array 1 [] in
                                             [list_var_read@more_var_read ;list_var_write@more_var_write] *)
  | Seq'(seq_lst), [list_var_read;list_var_write]->
                    map_stuffing_lists seq_lst var_name depth [list_var_read;list_var_write]
                    (* let current_run exp = stuffing_lists exp var_name depth [[];[]] in
                    let map_seq_of_results = List.map current_run seq_lst in
                    let more_var_read = extract_from_3d_array map_seq_of_results 0 [] in
                    let more_var_write = extract_from_3d_array map_seq_of_results 1 [] in
                    [list_var_read@more_var_read ;list_var_write@more_var_write] *)
                    
  | Or'(or_lst), [list_var_read;list_var_write]->
                    map_stuffing_lists or_lst var_name depth [list_var_read;list_var_write]
                    (* let current_run exp = stuffing_lists exp var_name depth [[];[]] in
                    let map_seq_of_results = List.map current_run or_lst in
                    let more_var_read = extract_from_3d_array map_or_of_results 0 [] in
                    let more_var_write = extract_from_3d_array map_or_of_results 1 [] in
                    [list_var_read@more_var_read ;list_var_write@more_var_write] *)

  | Def'(var1, val1), [list_var_read;list_var_write]-> stuffing_lists val1 var_name depth [list_var_read;list_var_write]
  
  | LambdaSimple'(params_str_lst, expr_tag_body), [list_var_read;list_var_write]->
                                (* TODO check - if: should_be_boxed is empty- then we won't return seq *)
                                (* let bady_rec = stuffing_lists expr_tag_body [[];[]] *)
                                (* [list_var_read;list_var_write] *)
                                stuffing_lists expr_tag_body var_name (depth+1) [list_var_read;list_var_write]

  | LambdaOpt'(params_str_lst, vs_str, expr_tag_body), [list_var_read;list_var_write]->
                                stuffing_lists expr_tag_body var_name (depth+1) [list_var_read;list_var_write]
  | Applic'(e1, args1) ,[list_var_read;list_var_write]-> 
                    let lst = [e1]@args1 in
                    map_stuffing_lists lst var_name depth [list_var_read;list_var_write]

  | ApplicTP'(e1, args1), [list_var_read;list_var_write]->
                    let lst = [e1]@args1 in
                    map_stuffing_lists lst var_name depth [list_var_read;list_var_write]

  | _ -> raise X_box_stuffing_lists 
  
  and map_stuffing_lists lst var_name depth lists = match lists with 
          | [list_var_read;list_var_write] ->
                    let current_run exp = stuffing_lists exp var_name depth [[];[]] in
                    let map_of_results = List.map current_run lst in
                    let more_var_read = extract_from_3d_array map_of_results 0 [] in
                    let more_var_write = extract_from_3d_array map_of_results 1 [] in
                    [list_var_read@more_var_read ;list_var_write@more_var_write]
          | _ -> raise X_map_stuffing_lists 
  in

  stuffing_lists expr var_name (-1) [[];[]] ;;

  




  (* res_lists = [[ref a; ref b; ref c] ; [ref a] ; [ref a ; ref d]]

  curr_list_ref = [ref a; ref b; ref c] *)

(* [ [[]] ; [[]]     ] *)
let extract_ribs_3d_array arr result = 
  let rec extraction arr result = match arr with
  | []  -> result
  | _ -> extraction (List.tl arr) result@(List.hd arr)
  in extraction arr result;;

let box_rib_stuffing expr var_name =
    let rec 
    
    expr var_name depth curr_list_ref res_lists  = match expr with
      | Const'(s1)-> res_lists
      | Var'(VarFree v1)-> res_lists
      | Var'(VarParam (v1,mn1))->
                              if ((String.equal v1 var_name) && (depth == (-1)))
                              then res_lists@[curr_list_ref]
                              else res_lists
    
      | Var'(VarBound (v1,mj1,mn1))-> 
                              if((String.equal v1 var_name) && (depth == mj1))
                              then res_lists@[curr_list_ref]
                              else res_lists
      | Set'(x,bval) ->

                  let var_type x = match x with
                    | VarParam (v1,mn1) ->
                                                    if ((String.equal v1 var_name) && (depth == (-1)))
                                                    then rib_stuffing bval var_name depth curr_list_ref res_lists@[curr_list_ref]
                                                    else res_lists
                    | VarBound (v1,mj1,mn1)-> 
                                                    if((String.equal v1 var_name) && (depth == mj1))
                                                    then rib_stuffing bval var_name depth curr_list_ref res_lists@[curr_list_ref]
                                                    else res_lists
                    | _ -> raise X_stuffing_lists_stuffing_lists
                    in
                    var_type x

      | If'(test_exp, then_exp, else_exp)-> 
                                            let if_lst = [test_exp; then_exp; else_exp] in
                                            map_stuffing_ribs if_lst var_name depth curr_list_ref res_lists
                                            
                                            
      | Seq'(seq_lst)-> map_stuffing_ribs seq_lst var_name depth curr_list_ref res_lists
                        
      | Or'(or_lst)-> map_stuffing_ribs or_lst var_name depth curr_list_ref res_lists
                       
      | Def'(var1, val1)-> rib_stuffing val1 var_name depth curr_list_ref res_lists
      
      | LambdaSimple'(params_str_lst, expr_tag_body)->
                            rib_stuffing expr_tag_body var_name (depth+1) (curr_list_ref@[(ref params_str_lst)]) res_lists
                                        
      | LambdaOpt'(params_str_lst, vs_str, expr_tag_body)->
                            rib_stuffing expr_tag_body var_name (depth+1) (curr_list_ref@[(ref params_str_lst)]) res_lists

      | Applic'(e1, args1)-> 
                            let lst = [e1]@args1 in
                            map_stuffing_ribs lst var_name depth curr_list_ref res_lists
    
      | ApplicTP'(e1, args1)->
                            let lst = [e1]@args1 in
                            map_stuffing_ribs lst var_name depth curr_list_ref res_lists
    
      | _ -> raise X_box_rib_stuffing 
      

    and map_stuffing_ribs lst var_name depth curr_list_ref res_lists = 
      
                let current_run exp = rib_stuffing exp var_name depth curr_list_ref [] in
                let map_of_results = List.map current_run lst in
                let more_var_ribs = extract_ribs_3d_array map_of_results [] in
                res_lists@more_var_ribs
      

      in
      rib_stuffing expr var_name (-1) [(ref ["to_remove"])] [];;





(* return the lst with intersection TODO we compare the major of varbound*)
let search_read_write_together list_read list_write = raise X_not_yet_implemented;;

(* return the lst with set for every var in lst *)
(* Set'(Var'(VarParam(v, minor)), Box'(VarParam(v,minor))) *)
let create_seq_boxset should_be_boxed = raise X_not_yet_implemented;;



(* let bady_gen_lists_rw = box expr_tag_body [] []
                            let should_be_boxed = search_read_write_together list_read list_write in
                            let seq_box_lst = create_seq_boxset should_be_boxed in 
                            let body_box = seq_box_lst@(change_var_with_box_set_get bady_rec should_be_boxed)) *)




let box_set expr = raise X_not_yet_implemented;;
(* 
let box_set expr = 
  let rec box expr =;;
  | Const'(s1)->
                      Const'(s1)
  | Var'(VarFree v1)->
                      Var'(VarFree v1)
  | Var'(VarParam (v1,mn1))->
                      Var'(VarParam (v1,mn1))
  | Var'(VarBound (v1,mj1,mn1))->
                      Var'(VarBound (v1,mj1,mn1))
  | Set'(x,bval) -> 
                      Set'(x,box bval)
  | If'(test_exp, then_exp, else_exp)-> 
                      If'(box test_exp, box then_exp, box else_exp)
  | Seq'(seq_lst)-> 
                      let map_box = List.map box seq_lst in
                      Seq'(map_box)
  | Or'(or_lst)-> 
                      let map_box = List.map box seq_lst in
                      Seq'(map_box)
  | Def'(var1, val1)->
                      Def'(var1, box val1)
  | LambdaSimple'(params_str_lst, expr_tag_body)->
                      let func_r_w var_name = (box_stuffing_lists expr_tag_body var_name) in
                      let params_r_w_lists = List.map func_r_w params_str_lst in
                      (* [ [[];[]]; [[];[]] ] *)
                      let func_is_r_w param = if ((List.length (List.hd param) > 0 ) && (List.length (List.hd (List.tl param)) > 0) ) 
                                              then true else false in
                      let is_read_write_together = List.map (func_is_r_w) params_r_w_lists in


                      let func_ribs var_namr = (box_rib_stuffing expr_tag_body var_name) in
                      let params_ribs_lists = List.map func_ribs params_str_lst in
                      
                      (* [[];[];[]] *)
                      let func_rest piece = (List.tl piece) in
                      let params_ribs_lists_clean_first_junk_func param_appearances = List.map func_rest param_appearances in
                      let params_ribs_cleaned = List.map params_ribs_lists_clean_first_junk_func params_ribs_lists in
                      (* [ [[];[];[]]; [[];[];[]] ] *)
                      let func_is_NOT_ribs_together param = in
                      let is_ribs_NOT_together = List.map (func_is_NOT_ribs_together) params_ribs_cleaned in
                        
                                    
  | LambdaOpt'(params_str_lst, vs_str, expr_tag_body)->
                        

  | Applic'(e1, args1)->
                        let map_box = List.map box args1 in
                        Applic'((box e1), map_box)
  | ApplicTP'(e1, args1)->
                        let map_box = List.map box args1 in
                        ApplicTP'((box e1), map_box)
  
  | _ -> raise X_box_rib_stuffing 


  in
  rib_stuffing expr var_name (-1) [(ref ["to_remove"])] [];;
 *)




let run_semantics expr = (annotate_tail_calls (annotate_lexical_addresses expr));;

(* 
let run_semantics expr =
  box_set
    (annotate_tail_calls
       (annotate_lexical_addresses expr));; *)
  
end;; (* struct Semantics *)
open Semantics;;




(* 





let tesst35_box = test_exp' ((LambdaSimple (["x"],
Applic (Var "list",
  [LambdaSimple ([], Var "x"); LambdaSimple (["y"], Set (Var "x", Var "y"))])))) 
  
  
(
LambdaSimple' (["x"],
Seq'
[Set' (Var' (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
ApplicTP' (Var' (VarFree "list"),
[LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)));
LambdaSimple' (["y"],
BoxSet' (VarBound ("x", 0, 0), Var' (VarParam ("y", 0))))])]));;


LambdaSimple' (["x"], 
ApplicTP' (Var' (VarFree "list"),
[LambdaSimple' ([], Var' (VarBound ("x", 0, 0)));
LambdaSimple' (["y"], Set' (VarBound ("x", 0, 0), Var' (VarParam ("y", 0))))]))




(lambda (x) (lambda () x (set! x 5)))
SAME

(lambda (x) 
  (lambda () x 
                (lambda()(set! x 5))))
SAME


(lambda (x) 
  x
  (lambda () x 
                (lambda()set! x 5)))
Different


(lambda (x) 
  (lambda () x 
                (lambda()set! x 5))
  (lambda () x 
                (lambda()set! x 5)))

Different


(lambda (x) 
  (if x 
        then (lambda()set! x 5))
        else x))
Different


(lambda (x) 
  (lambda () x 
                (lambda() x (set! x 5))))
SAME



(lambda (x) 
  x
  (lambda () x 
                (lambda() x (set! x 5))))
Different


chez = Pair(Symbol "lambda", Pair(Pair(Symbol "x", Nil), Pair(Symbol "x", Pair(Pair(Symbol "lambda", Pair(Nil, Pair(Symbol "x", Pair(Pair(Symbol "lambda", Pair(Nil, Pair(Symbol "x", Pair(Pair(Symbol "set!", Pair(Symbol "x", Pair(Number (Fraction(5, 1)), Nil))), Nil)))), Nil)))), Nil))))
tag_parser = LambdaSimple (["x"],Seq[Var "x"; LambdaSimple ([],Seq[Var "x";LambdaSimple ([],Seq [Var "x"; Set (Var "x", Const (Sexpr (Number (Fraction (5, 1)))))])])])
run_semantics = LambdaSimple' (["x"],Seq'[Var' (VarParam ("x", 0));LambdaSimple' ([],Seq'[Var' (VarBound ("x", 0, 0));LambdaSimple' ([],Seq'[Var' (VarBound ("x", 1, 0));Set' (VarBound ("x", 1, 0), Const' (Sexpr (Number (Fraction (5, 1)))))])])])

let a = box_rib_stuffing (Seq'[Var' (VarParam ("x", 0));LambdaSimple' ([],Seq'[Var' (VarBound ("x", 0, 0));LambdaSimple' ([],Seq'[Var' (VarBound ("x", 1, 0));Set' (VarBound ("x", 1, 0), Const' (Sexpr (Number (Fraction (5, 1)))))])])]) "x";;
- : string list ref list list =
[[{contents = ["to_remove"]}; {contents = []}; {contents = []}];
 [{contents = ["to_remove"]}; {contents = []}; {contents = []}];
 [{contents = ["to_remove"]}; {contents = []}]; 
 [{contents = ["to_remove"]}]]



 [[{contents = []}; {contents = []}];
  [{contents = []}; {contents = []}];
  [{contents = []}]; 
  []]


  val memq : 'a -> 'a list -> bool
  Same as List.mem, but uses physical equality instead of structural equality to compare list elements.



let pass_2 lst = List.map  func var_shows lst
[{contents = []}; {contents = []}]


let pass var_shows =       List.map  pass_2 lst
[{contents = []}; {contents = []}];

[{contents = []}; {contents = []}];

[{contents = []}]; 

[]
*)







let a var_shows = 
  let rec triangle_run answer var_shows = match var_shows with
  | [] -> answer
  | _ -> 
          let head = (List.hd var_shows) in
          let rest = (List.tl var_shows) in 
          let memq_param lst show = List.memq show lst in
          let func lst = List.map (memq_param lst) head in
          let bool_lst_lst_gen = List.map func rest in
          triangle_run (answer@bool_lst_lst_gen) rest 
  in
  let func_bool bool_lst = List.fold_left (||) false bool_lst in
  let bool_lst_of_ribs_Not = List.map  func_bool  (triangle_run [] var_shows)
  in
  let final_and = List.fold_left (&&) true bool_lst_of_ribs_Not in
  let is_ribs_to_box = not final_and in
  is_ribs_to_box;;






  val a : 'a list list -> bool list list 

  [[{contents = []}; {contents = []}];
  [{contents = []}; {contents = []}];
  [{contents = []}]; 
  []] 
let b = ref [];;
let c = ref [];;

a [[b; c]; [b; c];  [b];   []] ;;
- : bool list list = [[true; true]; [true; false]; [false; false]]   


[true; true; false]
NOT
[false; false; true]
true

(* let func_bool lst ans = match lst with
  | [] -> ans
  | _ -> func_bool (List.tl lst) (or ans (List.hd lst)) *)





(*  


(lambda (x) 
  (set! x 5)
  (lambda () x ))
Different


(lambda (x) 
  (set! x (+ x 1)))
Same RIBS

  (lambda (x) 
  (set! x (lambda()(+ x 1))))
  )
  Different RIBS 


(lambda (x) (set! x 5) (lambda () x ))
Pair(Symbol "lambda", Pair(Pair(Symbol "x", Nil), Pair(Pair(Symbol "set!", Pair(Symbol "x", Pair(Number (Fraction(5, 1)), Nil))), Pair(Pair(Symbol "lambda", Pair(Nil, Pair(Symbol "x", Nil))), Nil))))
LambdaSimple (["x"], Seq [Set (Var "x", Const (Sexpr (Number (Fraction (5, 1))))); LambdaSimple ([], Var "x")]) 
LambdaSimple' (["x"],Seq' [Set' (VarParam ("x", 0), Const' (Sexpr (Number (Fraction (5, 1))))); LambdaSimple' ([], Var' (VarBound ("x", 0, 0)))])

box_stuffing_lists 
(LambdaSimple' (["x"],Seq' [Set' (VarParam ("x", 0), Const' (Sexpr (Number (Fraction (5, 1))))); LambdaSimple' ([], Var' (VarBound ("x", 0, 0)))])) "x";;
- : expr' list list = [[]; []]


 
(* with -2 *)
box_rib_stuffing 
(LambdaSimple' (["x"],Seq' [Set' (VarParam ("x", 0), Const' (Sexpr (Number (Fraction (5, 1))))); LambdaSimple' ([], Var' (VarBound ("x", 0, 0)))])) "x";;
- : string list ref list list =
[[{contents = []}; {contents = ["x"]}; {contents = []}];
 [{contents = []}; {contents = ["x"]}]] 

 (* with -1 and you send the body of the lambda *)
 box_rib_stuffing 
(Seq' [Set' (VarParam ("x", 0), Const' (Sexpr (Number (Fraction (5, 1))))); LambdaSimple' ([], Var' (VarBound ("x", 0, 0)))]) "x";;

box_rib_stuffing 
(Seq' [Set' (VarParam ("x", 0), Const' (Sexpr (Number (Fraction (5, 1))))); LambdaSimple' ([], Var' (VarBound ("x", 0, 0)))]) "x";;
- : string list ref list list =                                                                                          [[{contents = ["TEMP_remove"]}; {contents = []}];                                                                         [{contents = ["TEMP_remove"]}]]   *)
