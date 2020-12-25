#use "semantic-analyser.ml";;


exception X_procces_const;;


(* This module is here for you convenience only!
   You are not required to use it.
   you are allowed to change it. *)
module type CODE_GEN = sig
  
  
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


  let basic_table = [(Sexpr(Nil), (1, "T_NIL")) , (Sexpr(Void), (2, "T_VOID")) , (Sexpr(Boolean), (1, "T_BOOL")) , (Sexpr(Boolean), (1, "T_BOOL"))] ;;

  let procces_const expr = 
    let rec consts expr arr = match expr with
      | Const' Void ->
      | Const'(Sexpr s1)->
      | Var'(VarFree v1)->
      | Var'(VarParam (v1,mn1))-> 
      | Var'(VarBound (v1,mj1,mn1))->
      | Box'(VarFree v1)-> 
      | Box'(VarParam (v1,mn1))-> 
      | Box'(VarBound (v1,mj1,mn1))->
      | BoxGet'(VarFree v1)->
      | BoxGet'(VarParam (v1,mn1))-> 
      | BoxGet'(VarBound (v1,mj1,mn1))->
      | BoxSet'(VarFree v1,e1) -> 
      | BoxSet'(VarParam (v1,mn1), e1)->
      | BoxSet'(VarBound (v1,mj1,mn1),e1) ->
      | If'(t1, th1, el1)->
      | Seq'(l1) ->
      | Or'(l1) -> 
      | Set'(var1, val1) ->
      | Def'(var1, val1)->
      | LambdaSimple'(vars1, body1)->
      | LambdaOpt'(vars1, var1, body1) ->
      | Applic'(e1, args1) ->
      | ApplicTP'(e1, args1) ->
      | _ -> raise X_procces_const;;


  let make_consts_tbl asts = List.map ;;


  let make_fvars_tbl asts = raise X_not_yet_implemented;;
  let generate consts fvars e = raise X_not_yet_implemented;;
end;;

