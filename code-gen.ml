#use "semantic-analyser.ml";;


exception X_procces_const;;



(* This module is here for you convenience only!
   You are not required to use it.
   you are allowed to change it. *)
module type CODE_GEN = sig

  

  val map_flatten_func : 'a list list -> 'a list -> 'a list
  val procces_const : expr' -> constant list
  val remove_duplicates_func : 'a list -> 'a list 
  val allocate_mem_func : constant list -> (constant * int * string) list 
  
  (* This signature assumes the structure of the constants table is
     a list of key-value pairs:
     - The keys are constant values (Sexpr(x) or Void)
     - The values are pairs of:
       * the offset from the base const_table address in bytes; and
       * a string containing the byte representation (or a sequence of nasm macros)
         of the constant value
     For example: [(Sexpr(Nil), (1, "T_NIL"))]
   *)
  val make_consts_tbl : expr' list -> (constant * (int * string)) list

  (* This signature assumes the structure of the fvars table is
     a list of key-value pairs:
     - The keys are the fvar names as strings
     - The values are the offsets from the base fvars_table address in bytes
     For example: [("boolean?", 0)]
   *)  
  val make_fvars_tbl : expr' list -> (string * int) list

  (* If you change the types of the constants and fvars tables, you will have to update
     this signature to match: The first argument is the constants table type, the second 
     argument is the fvars table type, and the third is an expr' that has been annotated 
     by the semantic analyser.
   *)
  val generate : (constant * (int * string)) list -> (string * int) list -> expr' -> string
end;;

module Code_Gen : CODE_GEN = struct
  (* let make_consts_tbl asts = raise X_not_yet_implemented;; *)





(* val map_flatten_func : 'a list list -> 'a list -> 'a list = <fun> *)

(* get a list and empty arr and return a flutten "map" *)
let rec map_flatten_func map res = match map with
  | [] -> res
  | _ ->  (map_flatten_func (List.tl map) (res@(List.hd map)) );;
  
let procces_const expr = 
  let rec consts expr arr = match expr with
    | Const'(s1)->          arr@[s1]
    | Var'(v1)->            arr
    | Box'(v1)->            arr
    | BoxGet'(v1)->         arr
    | BoxSet'(v1,e1) ->     (consts e1 arr)
    | If'(t1, th1, el1)->   (consts t1 arr) @ (consts th1 []) @ (consts el1 [])
    | Seq'(l1) ->
                            let apply_consts_with_map single = consts single [] in
                            let map_seq = List.map apply_consts_with_map l1 in
                            let flattened = (map_flatten_func map_seq []) in
                            arr@flattened
    | Or'(l1) ->
                            let apply_consts_with_map single = consts single [] in
                            let map_or = List.map apply_consts_with_map l1 in
                            let flattened = (map_flatten_func map_or []) in
                            arr@flattened
    | Set'(var, val1) ->     (consts val1 arr)
    | Def'(var, val1)->      (consts val1 arr)
    | LambdaSimple'(vars, body)->
                            (consts body arr)
    | LambdaOpt'(vars, var, body) ->
                            (consts body arr)
    | Applic'(proc, args) ->
                            let apply_consts_with_map single = consts single [] in
                            let map_args = List.map apply_consts_with_map args in
                            let flattened = (map_flatten_func map_args []) in
                            (consts proc arr) @ flattened
    | ApplicTP'(proc, args) ->
                            let apply_consts_with_map single = consts single [] in
                            let map_args = List.map apply_consts_with_map args in
                            let flattened = (map_flatten_func map_args []) in
                            (consts proc arr) @ flattened
    (* | _ -> raise X_procces_const *)
    in 
    consts expr [];;



    
    

let remove_duplicates_func arr_with_dups = 
  let rec remove_duplicates arr res = match arr with
  | [] -> res
  | _ ->  
          let head = (List.hd arr) in
          let tail = (List.tl arr) in
          let does_hd_exist_in_res = List.mem head res in
          if (does_hd_exist_in_res) then (remove_duplicates tail res) else (remove_duplicates tail (res@[head]))
  in 
  remove_duplicates arr_with_dups [];;


  

let allocate_mem_func arr_without_dups = 
  let rec allocate_mem arr res index = match arr with
  | [] -> res
  | _ ->  
          let head = (List.hd arr) in
          let tail = (List.tl arr) in 
          caten_head head tail res index

  and caten_head hd arr res index = match hd with
  | Void ->               (allocate_mem arr (res@[(Void, index, "MAKE_VOID")]) (index + 1))
  | Sexpr(Nil) ->         (allocate_mem arr (res@[(Sexpr(Nil), index, "MAKE_NIL")]) (index + 1))
  | Sexpr(Bool false) ->  (allocate_mem arr (res@[(Sexpr(Bool false), index, "MAKE_BOOL(0)")]) (index + 2))
  | Sexpr(Bool true) ->   (allocate_mem arr (res@[(Sexpr(Bool true), index, "MAKE_BOOL(1)")]) (index + 2))
  | Sexpr(Char(c1))->     (allocate_mem arr (res@[(hd, index, "MAKE_CHAR========================TODO")]) (index + 2))
  | Sexpr(Number(Float f1)) ->
                          (allocate_mem arr (res@[(hd, index, "MAKE_FLOAT========================TODO")]) (index + 9))
  | Sexpr(Number(Fraction (n1, d1))) ->
                          (allocate_mem arr (res@[(hd, index, "MAKE_FRAC========================TODO")]) (index + 17))
  | Sexpr(String(s1)) ->  (allocate_mem arr (res@[(hd, index, "MAKE_STRING========================TODO")]) (index + 9 + (String.length s1)))
  | Sexpr(Symbol(s1)) ->  (allocate_mem arr (res@[(hd, index, "MAKE_SYMBOL========================TODO")]) (index + 9))
  | Sexpr(Pair(car1, cdr1)) ->
                          (allocate_mem arr (res@[(hd, index, "MAKE_PAIR========================TODO")]) (index + 17))
  in
  allocate_mem arr_without_dups [] 0 ;;
  

      
  let make_consts_tbl asts = raise X_not_yet_implemented;;

  let make_fvars_tbl asts = raise X_not_yet_implemented;;
  let generate consts fvars e = raise X_not_yet_implemented;;
end;;
open Code_Gen;;


(* 
(LambdaSimple' (["x"],
  Seq'
  [Set' ( (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
    BoxGet' (VarParam ("x", 0));
     LambdaSimple' (["x"],
      Seq'
       [Set' ( (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
         BoxSet' (VarParam ("x", 0), Const' (Sexpr (Number (Fraction(1,1)))));
          LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)))]);
     LambdaSimple' ([],
      BoxSet' (VarBound ("x", 0, 0), BoxGet' (VarBound ("x", 0, 0))))])) *)

(* 

procces_const (
LambdaSimple' ([],
Seq'
[Const' (Sexpr (Bool(true)));
Const' (Sexpr (Number (Fraction(1,1))));

  Applic' (LambdaSimple' ([], Var' (VarFree "x")), []);
Applic'
(LambdaSimple' (["x"],
Seq'
[Set' ( (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
Const' (Sexpr (Nil));
Const' (Void);
Const' (Sexpr (Bool(true)));
BoxSet' (VarParam ("x", 0), Const' (Sexpr (Number (Fraction(1,1)))));
LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)))]),
[Const' (Sexpr (Number (Fraction(2,1))))]);
ApplicTP' (LambdaOpt' ([], "x", Var' (VarParam ("x", 0))),
[Const' (Sexpr (Number (Fraction(3,1))))])])
)
;;
 *)
