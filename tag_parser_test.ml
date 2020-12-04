
#use "reader.ml";;
open Reader;;
#use "tag-parser.ml";;
open Tag_Parser;;

let eq sexp_list1 sexp_list2 =
  let s1 = List.hd sexp_list1 in
  let s2 = List.hd sexp_list2 in
  sexpr_eq s1 s2;;


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


let rec test_exp res expected =
  match res ,expected with
  |[a;b],[c;d] -> if(expr_eq a c) then test_exp [b] [d] else false
  |[a],[c] -> if(expr_eq a c) then true else false


let test01 = test_exp (tag_parse_expressions([Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Number (Fraction(1,1)), Nil)), Pair (Pair (Symbol "y", Pair (Number (Fraction(2,1)), Nil)), Nil)), Pair (Symbol "y", Nil)))])) ([Applic (LambdaSimple (["x"; "y"], Var "y"),[Const (Sexpr (Number (Fraction(1,1)))); Const (Sexpr (Number ((Fraction(2,1)))))])]);;
 let test02 = test_exp (tag_parse_expressions([Pair (Symbol "let", Pair (Nil, Pair (Number (Fraction(10,1)), Nil)))])) ([Applic (LambdaSimple ([], Const (Sexpr (Number (Fraction(10,1))))), [])]);;
let test03 = test_exp (tag_parse_expressions([Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Number (Fraction(1,1)), Nil)), Nil), Pair (Symbol "x", Nil)))])) ([Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (Number (Fraction(1,1))))])]);;
let test04 = test_exp (tag_parse_expressions([Pair (Symbol "let", Pair (Nil, Pair (Pair (Symbol "begin", Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil)))), Nil)))])) ([Applic(LambdaSimple ([],Seq[Const (Sexpr (Number (Fraction(1,1)))); Const (Sexpr (Number (Fraction(2,1))));Const (Sexpr (Number (Fraction(3,1))))]),[])]);;
let test05 = test_exp (tag_parse_expressions([Pair (Symbol "let", Pair (Pair (Pair (Symbol "a", Pair (Number (Fraction(3,1)), Nil)), Pair (Pair (Symbol "b", Pair (Number (Fraction(10,1)), Nil)), Nil)), Pair (Pair (Symbol "+", Pair (Symbol "a", Pair (Symbol "b", Nil))), Nil)))])) ([Applic (LambdaSimple (["a"; "b"], Applic (Var "+", [Var "a"; Var "b"])),[Const (Sexpr (Number (Fraction(3,1)))); Const (Sexpr (Number (Fraction(10,1))))])]);;
let test07 = test_exp (tag_parse_expressions([Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (Char 'a', Nil)), Nil), Pair (Symbol "x", Nil)))])) ([Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (Char 'a'))])]);;

let test08 = test_exp (tag_parse_expressions([Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil), Pair (Pair (Symbol "define", Pair (Symbol "y", Pair (Number (Float 1.23), Nil))), Nil)))])) ([Applic(LambdaSimple (["x"], Def (Var "y", Const (Sexpr (Number (Float 1.23))))),[Const (Sexpr (String "asd"))])]);;
let test09 = test_exp (tag_parse_expressions([Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil), Pair (Pair (Symbol "begin", Pair (Pair (Symbol "define", Pair (Symbol "y", Pair (Number (Float 1.23), Nil))), Pair (Pair (Symbol "set", Pair (Symbol "y", Pair (Number (Fraction(-1,1)), Nil))), Nil))), Nil)))])) ([Applic(LambdaSimple (["x"],Seq[Def (Var "y", Const (Sexpr (Number (Float 1.23))));Applic (Var "set", [Var "y"; Const (Sexpr (Number (Fraction(-1,1))))])]),[Const (Sexpr (String "asd"))])]);;
let test10 = test_exp (tag_parse_expressions([Pair (Symbol "let", Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil), Pair (Pair (Symbol "begin", Pair (Symbol "x", Nil)), Nil)))])) ([Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (String "asd"))])]);;
let test11 = test_exp (tag_parse_expressions([Pair (Symbol "quasiquote",  Pair (Pair (Pair (Symbol "unquote", Pair (Symbol "x", Nil)), Nil), Nil))  ])) ([Applic (Var "cons", [Var "x"; Const (Sexpr Nil)])]);;

let test12 = test_exp (tag_parse_expressions([Pair (Symbol "quasiquote",Pair (Pair (Symbol "a", Pair (Symbol "b", Nil)), Nil))  ])) ([Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "cons", [Const (Sexpr (Symbol "b")); Const (Sexpr Nil)])])]);;
let test13 = test_exp (tag_parse_expressions([Pair (Symbol "quasiquote",Pair(Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)),Pair (Symbol "b", Nil)),Nil))])) ([Applic (Var "cons",[Var "a";Applic (Var "cons", [Const (Sexpr (Symbol "b")); Const (Sexpr Nil)])])]);;

let test14 = test_exp (tag_parse_expressions([Pair (Symbol "quasiquote",Pair(Pair (Symbol "a",Pair (Pair (Symbol "unquote", Pair (Symbol "b", Nil)), Nil)),Nil))])) ([Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "cons", [Var "b"; Const (Sexpr Nil)])])]);;
let test15 = test_exp (tag_parse_expressions([Pair (Symbol "quasiquote",Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)),Pair (Symbol "b", Nil)),Nil))])) ([Applic (Var "append",[Var "a";Applic (Var "cons", [Const (Sexpr (Symbol "b")); Const (Sexpr Nil)])])]);;
let test16 = test_exp (tag_parse_expressions([Pair (Symbol "quasiquote",Pair(Pair (Symbol "a",Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil)), Nil)),Nil))])) ([Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "append", [Var "b"; Const (Sexpr Nil)])])]);;
let test17 = test_exp (tag_parse_expressions([Pair (Symbol "quasiquote",Pair(Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)),Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil)), Nil)),Nil))])) ([Applic (Var "cons",[Var "a"; Applic (Var "append", [Var "b"; Const (Sexpr Nil)])])]);;
let test18 = test_exp (tag_parse_expressions([Pair (Symbol "quasiquote", Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)),Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil)), Nil)),Nil))])) ([Applic (Var "append",[Var "a"; Applic (Var "append", [Var "b"; Const (Sexpr Nil)])])]);;
 let test19  = test_exp (tag_parse_expressions([Pair (Symbol "quasiquote",Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)),Pair (Symbol "unquote", Pair (Symbol "b", Nil))),Nil))])) ([Applic (Var "append", [Var "a"; Var "b"])]);; 
 let test20 = test_exp (tag_parse_expressions([Pair (Symbol "quasiquote",Pair(Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)),Pair (Symbol "unquote-splicing", Pair (Symbol "b", Nil))),Nil))])) ([Applic (Var "cons", [Var "a"; Var "b"])]);; 


 let test21  = test_exp (tag_parse_expressions([Pair (Symbol "quasiquote",Pair(Pair(Pair(Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "a", Nil)), Nil),Nil),Nil),Nil))])) ([Applic (Var "cons",[Applic (Var "cons",[Applic (Var "append", [Var "a"; Const (Sexpr Nil)]); Const (Sexpr Nil)]);Const (Sexpr Nil)])]);;
let test22 = test_exp (tag_parse_expressions([Pair (Symbol "lambda", Pair (Nil, Pair (Number (Fraction(10,1)), Nil))) ])) ([LambdaSimple ([], Const (Sexpr (Number (Fraction(10,1)))))]);;
let test23 = test_exp (tag_parse_expressions([Pair (Symbol "lambda",Pair (Pair (Symbol "a", Pair (Symbol "b", Nil)), Pair (Symbol "a", Nil)))  ])) ([LambdaSimple (["a"; "b"], Var "a")]);;
let test24 = test_exp (tag_parse_expressions([Pair (Symbol "lambda", Pair (Pair (Symbol "a", Nil), Pair (Symbol "a", Nil)))  ])) ([LambdaSimple (["a"], Var "a")]);;
let test25 = test_exp (tag_parse_expressions([Pair (Symbol "lambda",Pair (Pair (Symbol "x", Pair (Symbol "y", Nil)),Pair (Pair (Symbol "+", Pair (Symbol "x", Pair (Symbol "y", Nil))), Nil)))])) ([LambdaSimple (["x"; "y"], Applic (Var "+", [Var "x"; Var "y"]))]);;
let test26 = test_exp (tag_parse_expressions([Pair (Symbol "lambda",Pair (Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil))),Pair(Pair (Symbol "if",Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil)))),Nil)))])) ([LambdaSimple (["x"; "y"; "z"], If (Var "x", Var "y", Var "z"))]);;
let test27 = test_exp (tag_parse_expressions([Pair (Symbol "lambda",Pair (Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil))),Pair(Pair (Symbol "begin",Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil)))),Nil)))])) ([LambdaSimple (["x"; "y"; "z"], Seq [Var "x"; Var "y"; Var "z"])]);;
let test28 = test_exp (tag_parse_expressions([Pair (Symbol "lambda",Pair (Pair (Symbol "x", Pair (Symbol "y", Nil)),Pair (Pair (Symbol "set", Pair (Symbol "x", Pair (Symbol "y", Nil))), Nil)))])) ([LambdaSimple (["x"; "y"], Applic (Var "set", [Var "x"; Var "y"]))]);;
let test29 = test_exp (tag_parse_expressions([Pair (Symbol "lambda",Pair(Pair (Symbol "x",Pair (Symbol "y", Pair (Symbol "z", Pair (Symbol "w", Nil)))),Pair(Pair (Symbol "if",Pair (Symbol "x",Pair (Pair (Symbol "+", Pair (Symbol "y", Pair (Symbol "z", Nil))),Pair (Pair (Symbol "+", Pair (Symbol "z", Pair (Symbol "w", Nil))), Nil)))),Nil)))])) ([LambdaSimple (["x"; "y"; "z"; "w"],If (Var "x", Applic (Var "+", [Var "y"; Var "z"]),Applic (Var "+", [Var "z"; Var "w"])))]);;
let test30 = test_exp (tag_parse_expressions([Pair (Symbol "lambda",Pair (Pair (Symbol "x", Pair (Symbol "y", Nil)),Pair(Pair (Symbol "or",Pair (Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "z", Nil))), Nil)),    Nil))) ])) ([LambdaSimple (["x"; "y"], Applic (Var "x", [Var "y"; Var "z"]))]);; 

let test31 = test_exp (tag_parse_expressions([Pair (Symbol "lambda",Pair (Pair (Symbol "x", Pair (Symbol "y", Symbol "vs")),Pair(Pair (Symbol "begin",Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "vs", Nil)))),Nil)))])) ([LambdaOpt (["x"; "y"], "vs", Seq [Var "x"; Var "y"; Var "vs"])]);;
let test32 = test_exp (tag_parse_expressions([Pair (Symbol "lambda",Pair (Pair (Symbol "x", Symbol "vs"),Pair (Pair (Symbol "if", Pair (Symbol "x", Pair (Symbol "vs", Nil))), Nil)))])) ([LambdaOpt (["x"], "vs", If (Var "x", Var "vs", Const Void))]);;
let test33 = test_exp (tag_parse_expressions([Pair (Symbol "lambda",Pair (Pair (Symbol "x", Pair (Symbol "y", Symbol "vs")),Pair(Pair (Symbol "and",Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil)))),Nil)))])) ([LambdaOpt (["x"; "y"], "vs",If (Const (Sexpr (Number (Fraction(1,1)))),If (Const (Sexpr (Number (Fraction(2,1)))), Const (Sexpr (Number (Fraction(3,1)))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))))]);;
let test34 = test_exp (tag_parse_expressions([Pair (Symbol "lambda",Pair(Pair (Symbol "a",Pair (Symbol "b", Pair (Symbol "c", Pair (Symbol "d", Symbol "vs")))),Pair(Pair (Symbol "if",Pair (Pair (Symbol ">", Pair (Symbol "a", Pair (Symbol "b", Nil))),Pair (Pair (Symbol "+", Pair (Symbol "c", Pair (Symbol "d", Nil))),Pair (Pair (Symbol "list", Pair (Symbol "vs", Nil)), Nil)))),Nil)))])) ([LambdaOpt (["a"; "b"; "c"; "d"], "vs",If (Applic (Var ">", [Var "a"; Var "b"]),Applic (Var "+", [Var "c"; Var "d"]), Applic (Var "list", [Var "vs"])))]);;
let test35 = test_exp (tag_parse_expressions([Pair (Symbol "lambda",Pair (Pair (Symbol "b", Symbol "vs"),Pair(Pair (Symbol "begin",Pair (Symbol "b",Pair(Pair (Symbol "define", Pair (Symbol "x", Pair (Number (Fraction(10,1)), Nil))),Pair(Pair (Symbol "set",Pair (Symbol "b",Pair(Pair (Symbol "+", Pair (Symbol "x", Pair (Number (Fraction(15,1)), Nil))),Nil))),Nil)))),Nil)))])) ([LambdaOpt (["b"], "vs",Seq[Var "b"; Def (Var "x", Const (Sexpr (Number (Fraction(10,1)))));Applic (Var "set",[Var "b"; Applic (Var "+", [Var "x"; Const (Sexpr (Number(Fraction(15,1))))])])])]);;
let test36 = test_exp (tag_parse_expressions([Pair (Symbol "lambda",Pair (Pair (Symbol "a", Pair (Symbol "b", Symbol "vs")),Pair(Pair (Symbol "cond",Pair (Pair (Symbol "a", Pair (Number (Fraction(1,1)), Nil)),Pair (Pair (Symbol "b", Pair (Number (Fraction(2,1)), Nil)),Pair(Pair (Symbol "else",Pair (Pair (Symbol "+", Pair (Symbol "a", Pair (Symbol "b", Nil))),Nil)),Nil)))),Nil)))])) ([LambdaOpt (["a"; "b"], "vs",If (Var "a", Const (Sexpr (Number (Fraction(1,1)))),If (Var "b", Const (Sexpr (Number (Fraction(2,1)))),Applic (Var "+", [Var "a"; Var "b"]))))]);;
let test37 = test_exp (tag_parse_expressions([Pair (Symbol "lambda",Pair (Pair (Symbol "x", Symbol "vs"), Pair (Symbol "vs", Nil)))   ])) ([LambdaOpt (["x"], "vs", Var "vs")]);;
let test38 = test_exp (tag_parse_expressions([Pair (Symbol "lambda",Pair (Pair (Symbol "x", Pair (Symbol "y", Symbol "vs")),Pair(Pair(Pair (Symbol "quasiquote",Pair(Pair (Pair (Symbol "unquote", Pair (Symbol "x", Nil)),Pair (Pair (Symbol "unquote-splicing", Pair (Symbol "y", Nil)), Nil)),Nil)),Nil),Nil)))])) ([LambdaOpt (["x"; "y"], "vs",Applic(Applic (Var "cons",[Var "x"; Applic (Var "append", [Var "y"; Const (Sexpr Nil)])]),[]))]);;
let test39 = test_exp (tag_parse_expressions([Pair (Symbol "lambda",Pair (Pair (Symbol "x", Pair (Symbol "y", Symbol "vs")),Pair(Pair (Symbol "and",Pair (Symbol "x", Pair (Symbol "y", Pair (Symbol "vs", Nil)))),Nil)))])) ([LambdaOpt (["x"; "y"], "vs",If (Var "x", If (Var "y", Var "vs", Const (Sexpr (Bool false))),Const (Sexpr (Bool false))))]);;



let test40= test_exp (tag_parse_expressions([Pair (Symbol "let*",Pair(Pair (Pair (Symbol "x", Pair (Number (Fraction(1,1)), Nil)),Pair (Pair (Symbol "y", Pair (Number (Fraction(2,1)), Nil)), Nil)),Pair (Symbol "y", Nil)))])) ([Applic(LambdaSimple (["x"],Applic (LambdaSimple (["y"], Var "y"), [Const (Sexpr (Number (Fraction(2,1))))])),[Const (Sexpr (Number (Fraction(1,1))))])]);;

 let test41 = test_exp (tag_parse_expressions([Pair (Symbol "let*",Pair(Pair (Pair (Symbol "x", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "y", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "z", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "a", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "b", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "c", Pair (Number (Fraction(10,1)), Nil)), Nil)))))),Pair(Pair (Symbol "begin",Pair (Symbol "x",Pair (Symbol "y",Pair (Symbol "z",Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil))))))),Nil)))])) ([Applic(LambdaSimple (["x"],Applic(LambdaSimple (["y"],Applic(LambdaSimple (["z"],Applic(LambdaSimple (["a"],Applic(LambdaSimple (["b"],Applic(LambdaSimple (["c"],Seq [Var "x"; Var "y"; Var "z"; Var "a"; Var "b"; Var "c"]),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])]);;
let test42 = test_exp (tag_parse_expressions([Pair (Symbol "let*",Pair(Pair (Pair (Symbol "a", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "b", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "c", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "d", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "e", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "f", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "g", Pair (Number (Fraction(10,1)), Nil)), Nil))))))),Pair(Pair (Symbol "and",Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c",Pair (Symbol "d",Pair (Symbol "e", Pair (Symbol "f", Pair (Symbol "g", Nil)))))))),Nil)))])) ([Applic(LambdaSimple (["a"],Applic(LambdaSimple (["b"],Applic(LambdaSimple (["c"],Applic(LambdaSimple (["d"],Applic(LambdaSimple (["e"],Applic(LambdaSimple (["f"],Applic(LambdaSimple (["g"],If (Var "a",If (Var "b",If (Var "c",If (Var "d",If (Var "e",If (Var "f", Var "g", Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false)))),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])]);;
let test43 = test_exp (tag_parse_expressions([Pair (Symbol "let*",Pair (Nil,Pair(Pair (Symbol "begin",Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil)))),Nil)))])) ([Applic(LambdaSimple ([],Seq[Const (Sexpr (Number (Fraction(1,1)))); Const (Sexpr (Number (Fraction(2,1))));Const (Sexpr (Number (Fraction(3,1))))]),[])]);;
let test44 = test_exp (tag_parse_expressions([Pair (Symbol "let*",Pair(Pair (Pair (Symbol "a", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "b", Pair (Number (Fraction(10,1)), Nil)), Nil)),Pair (Pair (Symbol "+", Pair (Symbol "a", Pair (Symbol "b", Nil))), Nil)))])) ([Applic(LambdaSimple (["a"],Applic (LambdaSimple (["b"], Applic (Var "+", [Var "a"; Var "b"])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])]);;
let test45 = test_exp (tag_parse_expressions([Pair (Symbol "let*",Pair (Pair (Pair (Symbol "x", Pair (Char 'a', Nil)), Nil),Pair (Symbol "x", Nil)))])) ([Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (Char 'a'))])]);;
let test46 = test_exp (tag_parse_expressions([Pair (Symbol "let*",Pair(Pair (Pair (Symbol "t", Pair (Bool true, Nil)),Pair (Pair (Symbol "th", Pair (Number (Fraction(10,1)), Nil)),Pair (Pair (Symbol "el", Pair (Number (Fraction(10,1)), Nil)), Nil))),Pair(Pair (Symbol "if",Pair (Bool true, Pair (Number (Fraction(10,1)), Pair (Number (Fraction(10,1)), Nil)))),Nil)))])) ([Applic(LambdaSimple (["t"],Applic(LambdaSimple (["th"],Applic(LambdaSimple (["el"],If (Const (Sexpr (Bool true)), Const (Sexpr (Number (Fraction(10,1)))),Const (Sexpr (Number (Fraction(10,1)))))),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Number (Fraction(10,1))))])),[Const (Sexpr (Bool true))])]);;
let test47 = test_exp (tag_parse_expressions([Pair (Symbol "let*",Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil),Pair(Pair (Symbol "define", Pair (Symbol "y", Pair (Number (Float 12.3), Nil))),Nil)))])) ([Applic(LambdaSimple (["x"], Def (Var "y", Const (Sexpr (Number (Float 12.3))))),[Const (Sexpr (String "asd"))])]);;
let test48 = test_exp (tag_parse_expressions([Pair (Symbol "let*",Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil),Pair(Pair (Symbol "begin",Pair(Pair (Symbol "define",Pair (Symbol "y", Pair (Number (Float 1.23), Nil))),Pair(Pair (Symbol "set", Pair (Symbol "y", Pair (Number (Fraction(10,1)), Nil))),Nil))),Nil)))])) ([Applic(LambdaSimple (["x"],Seq[Def (Var "y", Const (Sexpr (Number (Float 1.23))));Applic (Var "set", [Var "y"; Const (Sexpr (Number (Fraction(10,1))))])]),[Const (Sexpr (String "asd"))])]);;
let test49 = test_exp (tag_parse_expressions([Pair (Symbol "let*",Pair (Pair (Pair (Symbol "x", Pair (String "asd", Nil)), Nil),Pair (Pair (Symbol "begin", Pair (Symbol "x", Nil)), Nil)))])) ([Applic (LambdaSimple (["x"], Var "x"), [Const (Sexpr (String "asd"))])]);;
 


 let test100 = test_exp (tag_parse_expressions([Pair (Symbol "quote", Pair(Pair (Pair (Char '\t', Nil), Pair (Pair (Char '\r', Nil), Pair (Pair (Char ' ', Nil), Char '\000'))),Nil))])) ([Const(Sexpr(Pair (Pair (Char '\t', Nil), Pair (Pair (Char '\r', Nil), Pair (Pair (Char ' ', Nil), Char '\000')))))]);; 

 let test99 = test_exp (tag_parse_expressions([Pair (Symbol "quote",Pair (Pair (Number (Float 1.2), Pair (Number (Float 3.4), Nil)), Nil))])) ([Const (Sexpr (Pair (Number (Float 1.2), Pair (Number (Float 3.4), Nil))))]);;
 
 let test98= test_exp (tag_parse_expressions([Pair (Symbol "define",Pair (Pair (Symbol "pairing", Pair (Symbol "a", Pair (Symbol "b", Nil))),Pair(Pair (Symbol "quote",Pair (Pair (Symbol "a", Pair (Symbol "b", Nil)), Nil)),Nil)))])) ([Def (Var "pairing",LambdaSimple (["a"; "b"],Const (Sexpr (Pair (Symbol "a", Pair (Symbol "b", Nil))))))]);;
 let test97 = test_exp (tag_parse_expressions([Pair (Symbol "define",Pair(Pair (Symbol "if_fun",Pair (Symbol "if_test",Pair (Symbol "if_then", Pair (Symbol "if_else", Nil)))),Pair(Pair (Symbol "if",Pair (Symbol "if_test",Pair (Symbol "if_then", Pair (Symbol "if_else", Nil)))),Nil)))])) ([Def (Var "if_fun",LambdaSimple (["if_test"; "if_then"; "if_else"],If (Var "if_test", Var "if_then", Var "if_else")))]);;
 let test96 = test_exp (tag_parse_expressions([Pair (Symbol "define",Pair(Pair (Symbol "applic",Pair (Symbol "fun",Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c", Pair (Symbol "d", Pair (Symbol "e", Nil))))))),Pair(Pair (Symbol "fun",Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c", Pair (Symbol "d", Pair (Symbol "e", Nil)))))),Nil)))])) ([Def (Var "applic",LambdaSimple (["fun"; "a"; "b"; "c"; "d"; "e"],Applic (Var "fun", [Var "a"; Var "b"; Var "c"; Var "d"; Var "e"])))]);;
 let test95 = test_exp (tag_parse_expressions([Pair (Symbol "define",Pair (Pair (Symbol "returnonly", Pair (Symbol "x", Nil)),Pair(Pair (Symbol "begin",Pair (String "return only", Pair (Symbol "x", Nil))),Nil)))])) ([Def (Var "returnonly",LambdaSimple (["x"], Seq [Const (Sexpr (String "return only")); Var "x"]))]);;
 let test94 = test_exp (tag_parse_expressions([String ""])) ([Const (Sexpr (String ""))]);;
 let test93 = test_exp (tag_parse_expressions([Pair (Symbol "quote",Pair(Pair (Number (Fraction(1,1)),Pair (Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil)),Pair (Pair (Number (Fraction(4,1)), Pair (Number (Float 0.5), Nil)),Pair(Pair (Symbol "c",Pair (Symbol "d",Pair (Pair (Symbol "f", Symbol "g"), String "Hello World"))),Symbol "z")))),Nil))])) ([Const(Sexpr(Pair (Number (Fraction(1,1)),Pair (Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil)),Pair (Pair (Number (Fraction(4,1)), Pair (Number (Float 0.5), Nil)),Pair(Pair (Symbol "c",Pair (Symbol "d",Pair (Pair (Symbol "f", Symbol "g"), String "Hello World"))),Symbol "z"))))))]);;
 let test92 = test_exp (tag_parse_expressions([Pair (Symbol "quote",Pair(Pair (Number (Float 0.1),Pair (Number (Float (-0.2)),Pair (Number (Float 3.4),Pair (Number (Fraction(5,1)),Pair (Number (Float 6.7), Pair (Number (Fraction(8,1)), Number (Fraction(9,1)))))))),Nil))])) ([Const(Sexpr(Pair (Number (Float 0.1),Pair (Number (Float (-0.2)),Pair (Number (Float 3.4),Pair (Number (Fraction(5,1)),Pair (Number (Float 6.7), Pair (Number (Fraction(8,1)), Number (Fraction(9,1))))))))))]);;
 let test91 = test_exp (tag_parse_expressions([Pair (Symbol "quote",Pair(Pair (Number (Float (-0.2)),Pair (Number (Float 1.5),Pair(Pair (Number (Fraction(1,1)),Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil))),Pair (Pair (Number (Fraction(4,1)), Number (Fraction(6,1))),Pair (Pair (Symbol "a", Pair (Symbol "b", Symbol "c")),Pair (Symbol "d", Symbol "f")))))),Nil))])) ([Const(Sexpr(Pair (Number (Float (-0.2)),Pair (Number (Float 1.5),Pair(Pair (Number (Fraction(1,1)),Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil))),Pair (Pair (Number (Fraction(4,1)), Number (Fraction(6,1))),Pair (Pair (Symbol "a", Pair (Symbol "b", Symbol "c")),Pair (Symbol "d", Symbol "f"))))))))]);;
 let test90 = test_exp (tag_parse_expressions([Pair (Symbol "quote",Pair(Pair (String "a long long string",Pair (String "another long one",Pair(Pair (String "some numbers",Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Number (Fraction(3,1))))),Pair (String "Named Char", Char ' ')))),Nil))])) ([Const(Sexpr(Pair (String "a long long string",Pair (String "another long one",Pair(Pair (String "some numbers",Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Number (Fraction(3,1))))),Pair (String "Named Char", Char ' '))))))]);;
 let test89 = test_exp (tag_parse_expressions([Pair (Symbol "quote",Pair(Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c", Pair (Symbol "d", Pair (Symbol "f", Symbol "g"))))),Nil))])) ([Const(Sexpr(Pair (Symbol "a",Pair (Symbol "b",Pair (Symbol "c", Pair (Symbol "d", Pair (Symbol "f", Symbol "g")))))))]);;
 
 let test88 = test_exp (tag_parse_expressions([Pair (Symbol "quote",Pair(Pair (String "vary",Pair (String "long",Pair (Number (Fraction(0,1)),Pair (Number (Float 2.1),Pair (Number (Float (-4.2)),Pair (String "complex", Pair (Char '\r', Pair (String "list", Nil)))))))),Nil))])) ([Const(Sexpr(Pair (String "vary",Pair (String "long",Pair (Number (Fraction(0,1)),Pair (Number (Float 2.1),Pair (Number (Float (-4.2)),Pair (String "complex",Pair (Char '\r', Pair (String "list", Nil))))))))))]);;
 let test87 = test_exp (tag_parse_expressions([Pair (Symbol "quote",Pair (Pair (String "should", Pair (String "be", String "list")), Nil))])) ([Const (Sexpr (Pair (String "should", Pair (String "be", String "list"))))]);;
 let test86 = test_exp (tag_parse_expressions([Pair (String "should not", Pair (String "be", Pair (String "list", Nil)))])) ([Applic (Const (Sexpr (String "should not")),[Const (Sexpr (String "be")); Const (Sexpr (String "list"))])]);;
 let test85 = test_exp (tag_parse_expressions([Pair (Symbol "quote",Pair(Pair (String "hello",Pair (String "world",Pair (Pair (Number (Float 1.2), Number (Fraction(3,1))), Char '\000'))),Nil))])) ([Const(Sexpr(Pair (String "hello",Pair (String "world",Pair (Pair (Number (Float 1.2), Number (Fraction(3,1))), Char '\000')))))]);;
 let test84 = test_exp (tag_parse_expressions([Pair (Symbol "quote",Pair(Pair (String "a",Pair (String "b", Pair (Pair (String "c", String "d"), String "e"))),Nil))])) ([Const(Sexpr(Pair (String "a",Pair (String "b", Pair (Pair (String "c", String "d"), String "e")))))]);;
 let test83 = test_exp (tag_parse_expressions([Pair (Symbol "quote",Pair(Pair (Pair (Char '\t', Nil),Pair (Pair (Char '\r', Nil), Pair (Pair (Char ' ', Nil), Nil))),Nil))])) ([Const(Sexpr(Pair (Pair (Char '\t', Nil),Pair (Pair (Char '\r', Nil), Pair (Pair (Char ' ', Nil), Nil)))))]);;
 let test82 = test_exp (tag_parse_expressions([Pair (Symbol "quote",Pair(Pair (Number (Fraction(1,1)),Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Pair (Char '\012', Nil)))),Nil))])) ([Const(Sexpr(Pair (Number (Fraction(1,1)),Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Pair (Char '\012', Nil))))))]);;
 let test81 = test_exp (tag_parse_expressions([Char '\n'])) ([Const (Sexpr (Char '\n'))]);;
 let test80 = test_exp (tag_parse_expressions([Pair (Symbol "quote", Pair (Pair (Char '\000', Nil), Nil))])) ([Const (Sexpr (Pair (Char '\000', Nil)))]);;
 
 let test79 = test_exp (tag_parse_expressions([Pair (Symbol "quote", Pair (Pair (Number (Float 0.123), Nil), Nil))])) ([Const (Sexpr (Pair (Number (Float 0.123), Nil)))]);;
 let test78 = test_exp (tag_parse_expressions([Pair (Symbol "quote",Pair(Pair (Number (Float (-0.52)),Pair (Pair (Number (Float 0.5234), Number (Float (-123.4))),Number (Float (-0.535)))),Nil))])) ([Const(Sexpr(Pair (Number (Float (-0.52)),Pair (Pair (Number (Float 0.5234), Number (Float (-123.4))),Number (Float (-0.535))))))]);;
 let test77 = test_exp (tag_parse_expressions([Pair (Symbol "quote",Pair(Pair (Number (Float (-0.)),Pair (Number (Float 999.123),Pair (Pair (Number (Float 501.1), Number (Float 0.5)),Number (Float (-0.555))))),Nil))])) ([Const(Sexpr(Pair (Number (Float (-0.)),Pair (Number (Float 999.123),Pair (Pair (Number (Float 501.1), Number (Float 0.5)),Number (Float (-0.555)))))))]);;
 let test76 = test_exp (tag_parse_expressions([Pair (Symbol "quote",Pair(Pair(Pair (Number (Float (-0.5)),Pair (Number (Float 999.99), Pair (Number (Float (-12.1)), Nil))),Pair (Number (Float 10.),Pair (Pair (Number (Fraction(1,1)), Number (Fraction(2,1))), Number (Float 1.)))),Nil))])) ([Const(Sexpr(Pair(Pair (Number (Float (-0.5)),Pair (Number (Float 999.99), Pair (Number (Float (-12.1)), Nil))),Pair (Number (Float 10.),Pair (Pair (Number (Fraction(1,1)), Number (Fraction(2,1))), Number (Float 1.))))))]);;
 let test75 = test_exp (tag_parse_expressions([Pair (Symbol "quote",Pair (Pair (Number (Float 1.2), Number (Float 3.4)), Nil))])) ([Const (Sexpr (Pair (Number (Float 1.2), Number (Float 3.4))))]);;
 let test74 = test_exp (tag_parse_expressions([Pair (Symbol "quote",Pair(Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil))),Nil))])) ([Const(Sexpr(Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,1)), Pair (Number (Fraction(3,1)), Nil)))))]);;
 let test73 = test_exp (tag_parse_expressions([Pair (Symbol "define",Pair(Pair (Symbol "square",Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil)))),Pair(Pair (Symbol "",Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil)))),Nil)))])) ([Def (Var "square",LambdaSimple (["a"; "b"; "c"],Applic (Var "", [Var "a"; Var "b"; Var "c"])))]);;
 let test72 = test_exp (tag_parse_expressions([Pair (Symbol "define",Pair (Pair (Symbol "cmp", Pair (Symbol "a", Pair (Symbol "b", Nil))),Pair(Pair (Symbol "if",Pair (Pair (Symbol ">", Pair (Symbol "a", Pair (Symbol "b", Nil))),Pair (Number (Fraction(1,1)), Pair (Number (Fraction(-1,1)), Nil)))),Nil)))])) ([Def (Var "cmp",LambdaSimple (["a"; "b"],If (Applic (Var ">", [Var "a"; Var "b"]), Const (Sexpr (Number (Fraction(1,1)))),Const (Sexpr (Number (Fraction(-1,1)))))))]);;
 let test71 = test_exp (tag_parse_expressions([Pair (Symbol "lambda",Pair (Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Symbol "d"))),Pair(Pair (Symbol "quasiquote",Pair(Pair(Pair (Symbol "a",Pair (Pair (Symbol "unquote", Pair (Symbol "a", Nil)), Nil)),Pair(Pair (Symbol "b",Pair (Pair (Symbol "unquote", Pair (Symbol "b", Nil)), Nil)),Pair(Pair (Symbol "c",Pair (Pair (Symbol "unquote", Pair (Symbol "c", Nil)), Nil)),Pair(Pair (Symbol "d",Pair (Pair (Symbol "unquote", Pair (Symbol "d", Nil)), Nil)),Nil)))),Nil)),Nil)))])) ([LambdaOpt (["a"; "b"; "c"], "d",Applic (Var "cons",[Applic (Var "cons",[Const (Sexpr (Symbol "a"));Applic (Var "cons", [Var "a"; Const (Sexpr Nil)])]);Applic (Var "cons",[Applic (Var "cons",[Const (Sexpr (Symbol "b"));Applic (Var "cons", [Var "b"; Const (Sexpr Nil)])]);Applic (Var "cons",[Applic (Var "cons",[Const (Sexpr (Symbol "c"));Applic (Var "cons", [Var "c"; Const (Sexpr Nil)])]);Applic (Var "cons",[Applic (Var "cons",[Const (Sexpr (Symbol "d"));Applic (Var "cons", [Var "d"; Const (Sexpr Nil)])]);Const (Sexpr Nil)])])])]))]);;
 let test70 = test_exp (tag_parse_expressions([Pair (Symbol "define",Pair (Pair (Symbol "square", Pair (Symbol "x", Nil)),Pair (Pair (Symbol "", Pair (Symbol "x", Pair (Symbol "x", Nil))), Nil)))])) ([Def (Var "square",LambdaSimple (["x"], Applic (Var "", [Var "x"; Var "x"])))]);;
 



let testLetRec = test_exp (tag_parse_expressions([Pair (Symbol "letrec",Pair (Pair (Pair (Symbol "a", Pair (Number (Fraction (1, 1)), Nil)), Nil),
 Pair (Number (Fraction (1, 1)), Nil)))])) ([Applic(LambdaSimple (["a"],Seq[Set (Var "a", Const (Sexpr (Number (Fraction (1, 1)))));
     Applic (LambdaSimple ([], Const (Sexpr (Number (Fraction (1, 1))))), [])]),
 [Const (Sexpr (Symbol "whatever"))])]);;

let testLetRec2 = test_exp (tag_parse_expressions([Pair (Symbol "letrec",Pair (Pair (Pair (Symbol "a", Pair (Number (Fraction (1, 1)), Nil)), Nil),
 Pair (Number (Fraction (1, 1)), Pair (Symbol "a", Nil))))])) ([Applic(LambdaSimple (["a"],Seq[Set (Var "a", Const (Sexpr (Number (Fraction (1, 1)))));
     Applic(LambdaSimple ([],Seq [Const (Sexpr (Number (Fraction (1, 1)))); Var "a"]),[])]),
 [Const (Sexpr (Symbol "whatever"))])]);;

let testLetRec3 = test_exp (tag_parse_expressions([Pair (Symbol "letrec",Pair(Pair (Pair (Symbol "a", Pair (Number (Fraction (1, 1)), Nil)),
   Pair (Pair (Symbol "b", Pair (Number (Fraction (1, 1)), Nil)),Pair (Pair (Symbol "c", Pair (Number (Fraction (3, 1)), Nil)), Nil))),
 Pair (Number (Fraction (1, 1)), Pair (Symbol "a", Nil))))])) ([Applic(LambdaSimple (["a"; "b"; "c"],Seq[Set (Var "a", Const (Sexpr (Number (Fraction (1, 1)))));Set (Var "b", Const (Sexpr (Number (Fraction (1, 1)))));
     Set (Var "c", Const (Sexpr (Number (Fraction (3, 1)))));
     Applic
      (LambdaSimple ([],
        Seq [Const (Sexpr (Number (Fraction (1, 1)))); Var "a"]),
      [])]),
 [Const (Sexpr (Symbol "whatever")); Const (Sexpr (Symbol "whatever"));
  Const (Sexpr (Symbol "whatever"))])]);;

 
 let test55 = test_exp (tag_parse_expressions([Pair (Symbol "and", Pair (Number (Fraction(1,1)),Pair (Number (Fraction(1,1)),Pair (Number (Fraction(1,1)),Pair (Number (Fraction(1,1)),Pair (Number (Fraction(1,1)),Pair (Number (Fraction(1,1)),Pair (Number (Fraction(1,1)),Pair (Number (Fraction(1,1)),Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Nil)))))))))))])) ([If (Const (Sexpr (Number (Fraction(1,1)))),If (Const (Sexpr (Number (Fraction(1,1)))),If (Const (Sexpr (Number (Fraction(1,1)))),If (Const (Sexpr (Number (Fraction(1,1)))),If (Const (Sexpr (Number (Fraction(1,1)))),If (Const (Sexpr (Number (Fraction(1,1)))),If (Const (Sexpr (Number (Fraction(1,1)))),If (Const (Sexpr (Number (Fraction(1,1)))),If (Const (Sexpr (Number (Fraction(1,1)))), Const (Sexpr (Number (Fraction(1,1)))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false))),Const (Sexpr (Bool false)))]);;  
 
 
 let test54 = test_exp (tag_parse_expressions([Pair (Symbol "cond",Pair (Pair (Symbol "a", Pair (Symbol "=>", Pair (Symbol "b", Nil))),Pair(Pair (Symbol "else",Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Nil)))),Nil)))])) ([Applic(LambdaSimple (["value"; "f"; "rest"],If (Var "value", Applic (Applic (Var "f", []), [Var "value"]),Applic (Var "rest", []))),[Var "a"; LambdaSimple ([], Var "b");LambdaSimple ([],Seq[Const (Sexpr (Number (Fraction(1,1)))); Const (Sexpr (Number (Fraction(1,1))));Const (Sexpr (Number (Fraction(1,1))))])])]);; 
 let test53 = test_exp (tag_parse_expressions([Pair (Symbol "cond",Pair (Pair (Symbol "a", Pair (Symbol "=>", Pair (Symbol "b", Nil))), Nil))])) ([Applic(LambdaSimple (["value"; "f"],If (Var "value", Applic (Applic (Var "f", []), [Var "value"]),Const Void)),[Var "a"; LambdaSimple ([], Var "b")])]);;
  let test52 = test_exp (tag_parse_expressions([Pair (Symbol "cond",Pair(Pair (Number (Fraction(1,1)), Pair (Number (Fraction(2,2)), Pair (Number (Fraction(3,3)), Nil))),Pair(Pair (Symbol "else",Pair (Number (Fraction(4,4)), Pair (Number (Fraction(5,5)), Pair (Number (Fraction(6,6)), Nil)))),Pair(Pair (Number (Fraction(7,7)), Pair (Number (Fraction(8,8)), Pair (Number (Fraction(9,9)), Nil))),Nil))))])) ([If (Const (Sexpr (Number (Fraction(1,1)))),Seq [Const (Sexpr (Number (Fraction(2,2)))); Const (Sexpr (Number (Fraction(3,3))))],Seq[Const (Sexpr (Number (Fraction(4,4)))); Const (Sexpr (Number (Fraction(5,5))));Const (Sexpr (Number (Fraction(6,6))))])]);; 
 let test51 = test_exp (tag_parse_expressions([Pair (Symbol "cond",Pair(Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Nil))),Pair(Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Nil))),Pair(Pair (Symbol "else",Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Nil)))),Nil))))])) ([If (Const (Sexpr (Number (Fraction(1,1)))),Seq [Const (Sexpr (Number (Fraction(1,1)))); Const (Sexpr (Number (Fraction(1,1))))],If (Const (Sexpr (Number (Fraction(1,1)))),Seq [Const (Sexpr (Number (Fraction(1,1)))); Const (Sexpr (Number (Fraction(1,1))))],Seq[Const (Sexpr (Number (Fraction(1,1)))); Const (Sexpr (Number (Fraction(1,1))));Const (Sexpr (Number (Fraction(1,1))))]))]);; 
 let test50 = test_exp (tag_parse_expressions([Pair (Symbol "cond",Pair(Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Nil))),Pair(Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Pair (Number (Fraction(1,1)), Nil))),Nil)))])) ([If (Const (Sexpr (Number (Fraction(1,1)))),Seq [Const (Sexpr (Number (Fraction(1,1)))); Const (Sexpr (Number (Fraction(1,1))))],If (Const (Sexpr (Number (Fraction(1,1)))),Seq [Const (Sexpr (Number (Fraction(1,1)))); Const (Sexpr (Number (Fraction(1,1))))],Const Void))]);;
 

(* let bool1 = test_exp (read_sexprs(" #t  ")) ([Bool(true)]);; *)





