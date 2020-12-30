#use "semantic-analyser.ml";;


exception X_procces_const;;
exception X_return_address_in_const_table;;

exception X_generate;;



(* This module is here for you convenience only!
   You are not required to use it.
   you are allowed to change it. *)
module type CODE_GEN = sig

  

  val map_flatten_func : 'a list list -> 'a list -> 'a list
  val procces_const : expr' -> constant list
  val remove_duplicates_func : 'a list -> 'a list 
  val allocate_mem_func : constant list -> (constant * (int * string)) list 
  
  (* This signature assumes the structure of the constants table is
     a list of key-value pairs:
     - The keys are constant values (Sexpr(x) or Void)
     - The values are pairs of:
       * the offset from the base const_table address in bytes; and
       * a string containing the byte representation (or a sequence of nasm macros)
         of the constant value
     For example: [(Sexpr(Nil), (1, "T_NIL"))]
   *)
  val return_address_in_const_table_func : ('a * (int * 'b)) list -> 'a -> string

  val make_consts_tbl : expr' list -> (constant * (int * string)) list

  (* This signature assumes the structure of the fvars table is
     a list of key-value pairs:
     - The keys are the fvar names as strings
     - The values are the offsets from the base fvars_table address in bytes
     For example: [("boolean?", 0)]
   *)  


  val procces_fvar : expr' -> string list
  val allocate_mem_fvar_func : 'a list -> ('a * int) list
  val make_fvars_tbl : expr' list -> (string * int) list

  (* If you change the types of the constants and fvars tables, you will have to update
     this signature to match: The first argument is the constants table type, the second 
     argument is the fvars table type, and the third is an expr' that has been annotated 
     by the semantic analyser.
   *)
  val generate : (constant * (int * string)) list -> (string * int) list -> expr' -> string
  val generate_helper : (constant * (int * string)) list -> (string * int) list -> expr' -> string
  val run_gen : expr' list -> string 
end;;

module Code_Gen : CODE_GEN = struct
  (* let make_consts_tbl asts = raise X_not_yet_implemented;; *)





  (* val return_address_in_const_table_func : ('a * (int * 'b)) list -> 'a -> string *)

  let return_address_in_const_table_func table const_param =
    let rec return_address_in_const_table table const_param = match table with
    | [] -> raise X_return_address_in_const_table
    | _ ->  check_hd_table (List.hd table) (List.tl table) const_param

    and check_hd_table head tail const_param =
      let (a,(b,c)) = head in
      (* match a with *)
      (* | const_param -> (string_of_int b)
      | _ ->return_address_in_const_table tail const_param *)
      if (a = const_param) then (string_of_int b) else return_address_in_const_table tail const_param

    in return_address_in_const_table table const_param;;


    (* return index in fvar_table_func *)
  let return_index_in_fvar_table_func table fvar_param =
    let rec return_address_in_fvar_table table fvar_param = match table with
    | [] -> raise X_return_address_in_const_table
    | _ ->  check_hd_table (List.hd table) (List.tl table) fvar_param

    and check_hd_table head tail fvar_param =
      let (a,b) = head in
      if (a = fvar_param) then (string_of_int b) else return_address_in_fvar_table tail fvar_param

    in return_address_in_fvar_table table fvar_param;;





(* val map_flatten_func : 'a list list -> 'a list -> 'a list = <fun> *)

(* get a list and empty arr and return a flutten "map" *)
(* for symbol we insert his string and the relevant symbol that point him*)
(* for pair we insert recursivly car and cdr, and then insert the pair as well *)
let rec map_flatten_func map res = match map with
  | [] -> res
  | _ ->  (map_flatten_func (List.tl map) (res@(List.hd map)) );;
  
let procces_const expr = 
  let rec consts expr arr = match expr with
    | Const'(Sexpr(Symbol(s1)))-> 
                            arr@[(Sexpr(String(s1))); (Sexpr(Symbol(s1)))]
    | Const'(Sexpr(Pair(car, cdr)))->
                            arr@ (consts (Const'(Sexpr(car))) []) @ (consts (Const'(Sexpr(cdr))) []) @ [(Sexpr(Pair(car, cdr)))]
    
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
  | Void ->               (allocate_mem arr (res@[(Void, (index, "MAKE_CONST_VOID\n"))]) (index + 1))
  | Sexpr(Nil) ->         (allocate_mem arr (res@[(Sexpr(Nil), (index, "MAKE_CONST_NIL\n"))]) (index + 1))
  | Sexpr(Bool false) ->  (allocate_mem arr (res@[(Sexpr(Bool false), (index, "MAKE_LITERAL_BOOL(0)\n"))]) (index + 2))
  | Sexpr(Bool true) ->   (allocate_mem arr (res@[(Sexpr(Bool true), (index, "MAKE_LITERAL_BOOL(1)\n"))]) (index + 2))
  | Sexpr(Char(c1))->     (allocate_mem arr (res@[(hd, (index,( String.concat "" ["MAKE_LITERAL_CHAR(";Char.escaped c1;")\n"])))]) (index + 2))
  | Sexpr(Number(Float f1)) ->
                          (allocate_mem arr (res@[(hd, (index,( String.concat "" ["MAKE_LITERAL_FLOAT(";(string_of_float f1);")\n"])))]) (index + 9))
  | Sexpr(Number(Fraction (n1, d1))) ->
                          (allocate_mem arr (res@[(hd, (index,( String.concat "" ["MAKE_LITERAL_RATIONAL("; string_of_int n1; ", "; string_of_int d1; ")\n"])))]) (index + 17))
  | Sexpr(String(s1)) ->  (allocate_mem arr (res@[(hd, (index,( String.concat "" ["MAKE_LITERAL_STRING ";s1; "\n"])))]) (index + 9 + (String.length s1)))
  (* | Sexpr(String(s1)) ->  (allocate_mem arr (res@[(hd, (index,( String.concat "" ["MAKE_LITERAL_STRING(";s1;")"])))]) (index + 9 + (String.length s1))) *)
  | Sexpr(Symbol(s1)) ->  
                          let address_of_the_string = return_address_in_const_table_func res (Sexpr(String(s1))) in
                          (allocate_mem arr (res@[(hd, (index,( String.concat "" ["MAKE_LITERAL_SYMBOL(const_tbl+";address_of_the_string;")\n"])))]) (index + 9))
  
  | Sexpr(Pair(car, cdr)) ->
                          let address_of_the_car = (return_address_in_const_table_func res (Sexpr(car))) in   
                          let address_of_the_cdr = (return_address_in_const_table_func res (Sexpr(cdr))) in
                          (allocate_mem arr (res@[(hd, (index,( String.concat "" ["MAKE_LITERAL_PAIR(const_tbl+"; address_of_the_car; ",const_tbl+"; address_of_the_cdr; ")\n"])))]) (index + 17))
  in
  allocate_mem arr_without_dups [] 0 ;;
  

      
  (* let make_consts_tbl asts = (allocate_mem_func (remove_duplicates_func (procces_const asts) ));; *)
  let make_consts_tbl asts = 
    let add_void_nil_boolTF = [Void;Sexpr(Nil);Sexpr(Bool false);Sexpr(Bool true)] 
    in
    (allocate_mem_func (remove_duplicates_func  (add_void_nil_boolTF @ ( map_flatten_func (List.map procces_const asts) []) )));;
  
  (* let make_consts_tbl asts = raise X_not_yet_implemented;; *)

  (*let make_consts_tbl asts = raise X_not_yet_implemented;; *)




  let procces_fvar expr = 
    let rec fvar expr arr = match expr with
      | Const'(s1)->          arr
      | Var'(VarFree v1) ->   arr@[v1]
      | Var'(v1) ->           arr
      | Box'(v1)->            arr
      | BoxGet'(v1)->         arr
      | BoxSet'(v1,e1) ->     (fvar e1 arr)
      | If'(t1, th1, el1)->   (fvar t1 arr) @ (fvar th1 []) @ (fvar el1 [])
      | Seq'(l1) ->
                              let apply_fvar_with_map single = fvar single [] in
                              let map_seq = List.map apply_fvar_with_map l1 in
                              let flattened = (map_flatten_func map_seq []) in
                              arr@flattened
      | Or'(l1) ->
                              let apply_fvar_with_map single = fvar single [] in
                              let map_or = List.map apply_fvar_with_map l1 in
                              let flattened = (map_flatten_func map_or []) in
                              arr@flattened
      | Set'(var, val1) ->     (fvar val1 arr)
      | Def'(var, val1)->      (fvar val1 arr)
      | LambdaSimple'(vars, body)->
                              (fvar body arr)
      | LambdaOpt'(vars, var, body) ->
                              (fvar body arr)
      | Applic'(proc, args) ->
                              let apply_fvar_with_map single = fvar single [] in
                              let map_args = List.map apply_fvar_with_map args in
                              let flattened = (map_flatten_func map_args []) in
                              (fvar proc arr) @ flattened
      | ApplicTP'(proc, args) ->
                              let apply_fvar_with_map single = fvar single [] in
                              let map_args = List.map apply_fvar_with_map args in
                              let flattened = (map_flatten_func map_args []) in
                              (fvar proc arr) @ flattened
      (* | _ -> raise X_procces_const *)
      in 
      fvar expr [];;


  let allocate_mem_fvar_func arr_without_dups = 
    let rec allocate_mem arr res index = match arr with
    | [] -> res
    | _ ->  
            let head = (List.hd arr) in
            let tail = (List.tl arr) in 
            caten_head head tail res index
  
    and caten_head hd arr res index = (allocate_mem arr (res@[(hd, (index * 8))]) (index + 1))
    in
    allocate_mem arr_without_dups [] 0 ;;



    let make_fvars_tbl asts =  (allocate_mem_fvar_func (remove_duplicates_func ( map_flatten_func  (List.map procces_fvar asts) []) ));;





  (* let make_fvars_tbl asts = raise X_not_yet_implemented;; *)
  (* let generate consts fvars e = raise X_not_yet_implemented;; *)






    

  let generate_helper consts fvars e =

    let rec generate_func consts fvars e index = match e with
    | Const'(s1)->          String.concat "" ["mov rax, const_tbl+"; (return_address_in_const_table_func consts s1);"\n"]
    | Var'(VarParam(var_name,minor)) -> 
                            String.concat "" ["mov rax, qword [rbp + 8 ∗ (4 + ";(string_of_int minor);")]\n"]
    | Var'(VarBound (v1,major,minor))->
                            String.concat "" ["mov rax, qword [rbp + 8 ∗ 2]\n"; 
                                              "mov rax, qword [rax + 8 ∗"; (string_of_int major);"]\n";
                                              "mov rax, qword [rax + 8 ∗"; (string_of_int minor);"]\n"]
                            
    | Var'(VarFree v1) ->   
                            let labelInFVarTable = return_index_in_fvar_table_func fvars v1 in
                            String.concat "" ["mov rax, qword [ fval_tbl + (";(labelInFVarTable);" * WORD_SIZE)]\n"]
    
    | Box'(v1)-> raise X_procces_const

    | BoxGet'(v1)->         String.concat "" [generate_func consts fvars (Var'(v1)) index;
                                              "mov rax, qword [rax]\n"]
                                              
    | BoxSet'(v1,e1) ->     String.concat "" [generate_func consts fvars e1 index;
                                              "push rax\n";
                                              generate_func consts fvars (Var'(v1)) index;
                                              "pop qword [rax]\n";
                                              "mov rax, SOB_VOID\n"]
   
    | If'(t1, th1, el1)->   let updated_index = (index + 1) in
                            String.concat "" [generate_func consts fvars t1 updated_index;
                                              "cmp rax, sob_false\n";
                                              "je Lelse";  (string_of_int updated_index); "\n";
                                              generate_func consts fvars th1 updated_index;
                                              "jmp Lexit";(string_of_int updated_index);"\n";
                                              "Lelse"; (string_of_int updated_index); ":\n";
                                              generate_func consts fvars el1 updated_index;
                                              "Lexit"; (string_of_int updated_index); ":\n";]


    | Seq'(e_lst) ->        (generate_seq consts fvars e_lst "" index)
    | Or'(or_lst) ->        (generate_or consts fvars or_lst "" (index + 1))
    | Set'(VarParam(var_name,minor), c) -> 
                            String.concat "" [(generate_func consts fvars c index); "\n";
                            "mov qword [rbp + 8 * (4 + "; (string_of_int minor); ")],rax\n";
                            "mov rax, SOB_VOID\n"]
    | Set'(VarBound (v1,major,minor), c) -> 
                            String.concat "" [(generate_func consts fvars c index); "\n";
                            "mov rbx, qword [rbp + 8 ∗ 2]\n";
                            "mov rbx, qword [rbx + 8 ∗ "; (string_of_int major);"]\n";
                            "mov qword [rbx + 8 ∗ "; (string_of_int minor);"], rax\n";
                            "mov rax, SOB_VOID\n"]
    | Set'(VarFree v1, c) -> 
                            define_exp_set_fvar consts fvars v1 c index

    | Def'(VarFree v1, val1)->
                            define_exp_set_fvar consts fvars v1 val1 index
                            
    
    (* | LambdaSimple'(vars, body)-> raise X_procces_const *)
                            
    | LambdaOpt'(vars, var, body) -> raise X_procces_const
                            
    | Applic'(proc, args) -> 
                            (applics consts fvars proc args index)
                            
    | ApplicTP'(proc, args) -> 
                            (applics consts fvars proc args index)
                            
    | _ -> raise X_generate
    



    (* and generate_with_parenthesis consts fvars e = String.concat "" ["[";generate_func consts fvars e;"]"] *)

    and generate_seq consts fvars e_lst res index = match e_lst with
    | [] -> res
    | _ ->  generate_seq consts fvars (List.tl e_lst) (String.concat "" [res; generate_func consts fvars (List.hd e_lst) index]) index


    and generate_or consts fvars e_lst res index = match e_lst with
    | [] -> (String.concat "" [res;"Lexit";(string_of_int index); ":\n" ])
    | _ ->  generate_seq consts fvars (List.tl e_lst) (String.concat "" [res; generate_func consts fvars (List.hd e_lst) index;
                                                                         "cmp rax, sob_false\n";
                                                                         "jne Lexit";(string_of_int index); "\n"]) index

    and push_applic_args consts fvars app_lst res index = match app_lst with
    | [] -> res
    | _ ->  (push_applic_args consts fvars (List.tl app_lst) (String.concat "" [res; generate_func consts fvars (List.hd app_lst) index;
                                                                                "push rax\n"]) index)
    
    and applics consts fvars proc args index =
        let num_args = (List.length args) in
        let reversed_args = List.rev args in
        let push_args = push_applic_args consts fvars reversed_args "" index in
        let push_n = (String.concat "" [push_args; "push "; (string_of_int num_args); "\n" ;
                                        (generate_func consts fvars proc index);
                                        "TODO Verify that rax has type closure\n";
                                        "TODO push rax→ env";
                                        "TODO call rax→ code";
                                        " SLIDE 96" ])
        in push_n


    and define_exp_set_fvar consts fvars v c index = 
                            let labelInFVarTable = return_index_in_fvar_table_func fvars v in

                            String.concat "" [(generate_func consts fvars c index); "\n";
                            "mov qword [ fval_tbl + (";labelInFVarTable;" * WORD_SIZE)], rax\n";
                            "mov rax, SOB_VOID\n"]

    in
    generate_func consts fvars e 0;;

  let generate consts fvars e = generate_helper consts fvars e;;



  let run_gen expr_lst =
    let constable = make_consts_tbl expr_lst in
    let fvar_table = make_fvars_tbl expr_lst  in
    generate_helper constable fvar_table (List.hd expr_lst);;
    

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

(* 
 make_consts_tbl
 ([(LambdaSimple' ([],
 Seq'
 [Const' (Sexpr (String("moshe_hagever")));
 Const' (Sexpr (Number (Fraction(1,1))));
 Const'(Sexpr(Pair(  (String("moshe")), (Number (Fraction(2,1))) )));
 
   Applic' (LambdaSimple' ([], Var' (VarFree "x")), []);
 Applic'
 (LambdaSimple' (["x"],
 Seq'
 [Set' ( (VarParam ("x", 0)), Box' (VarParam ("x", 0)));
 Const' (Sexpr (Nil));
 Const' (Sexpr(Symbol("moshe_hagever")));
 Const' (Void);
 Const' (Sexpr (Bool(true)));
 BoxSet' (VarParam ("x", 0), Const' (Sexpr (Number (Fraction(1,1)))));
 LambdaSimple' ([], BoxGet' (VarBound ("x", 0, 0)))]),
 [Const' (Sexpr (Number (Fraction(2,1))))]);
 ApplicTP' (LambdaOpt' ([], "x", Var' (VarParam ("x", 0))),
 [Const' (Sexpr (Number (Fraction(3,1))))])]));

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
      BoxSet' (VarBound ("x", 0, 0), BoxGet' (VarBound ("x", 0, 0))))]))
 
 ])
  *)

(* 

  make_fvars_tbl ([
  Applic' (Var' (VarFree "y"),
 [LambdaSimple' (["y"],
   Seq'
    [Set'(VarParam ("y", 0), Box' (VarParam ("y", 0)));
      Set' (VarFree "a",
        LambdaSimple' (["b"],
         ApplicTP' (Var' (VarFree "a"), [Var' (VarParam ("b", 0))])));
       Set' (VarFree "t",
        LambdaSimple' (["x"],
         Seq'
          [BoxSet' (VarBound ("y", 0, 0),
            LambdaSimple' (["j"],
             ApplicTP' (Var' (VarBound ("x", 0, 0)),
              [Var' (VarParam ("j", 0)); Var' (VarBound ("x", 0, 0))])));
           Var' (VarFree "h")]));
       ApplicTP' (BoxGet' (VarParam ("y", 0)), [Var' (VarFree "a")])])])
  ]);;
- : (string * int) list = [("y", 0); ("a", 8); ("h", 16)]         *)

(* 
[Seq'
    [Set'(VarParam ("y", 0), Var'(VarParam ("y", 0)));
      Set' (VarFree "a",
         ApplicTP' (Var' (VarFree "a"), [Var' (VarParam ("b", 0))]))]]
          *)

(* 
return_address_in_const_table_func ([(Sexpr (String "moshe"), (0, "MAKE_LITERAL_STRING moshe\n"));
 (Sexpr (Number (Fraction (1, 1))), (14, "MAKE_LITERAL_RATIONAL(1, 1)\n"));
 (Sexpr Nil, (31, "MAKE_CONST_NIL\n")); (Void, (32, "MAKE_CONST_VOID\n"));
 (Sexpr (Bool true), (33, "MAKE_LITERAL_BOOL(1)\n"));
 (Sexpr (Number (Fraction (2, 1))), (35, "MAKE_LITERAL_RATIONAL(2, 1)\n"));
 (Sexpr (Number (Fraction (3, 1))), (52, "MAKE_LITERAL_RATIONAL(3, 1)\n"))])
(Sexpr(Number (Fraction (1, 1))));; *)