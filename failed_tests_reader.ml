
#use "reader.ml";;
  open Reader;;
  
  let eq sexp_list1 sexp_list2 =
    let s1 = List.hd sexp_list1 in
    let s2 = List.hd sexp_list2 in
    sexpr_eq s1 s2;;
  
  
    let rec sexpr_eq s1 s2 =
      match s1, s2 with
      | Bool(b1), Bool(b2) -> b1 = b2
      | Nil, Nil -> true
      | Number(Float f1), Number(Float f2) -> abs_float(f1 -. f2) < 0.001
      | Number(Fraction (n1, d1)), Number(Fraction (n2, d2)) -> n1 = n2 && d1 = d2
      | Char(c1), Char(c2) -> c1 = c2
      | String(s1), String(s2) -> s1 = s2
      | Symbol(s1), Symbol(s2) -> s1 = s2
      | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
      | _ -> false;;
  
  
  let rec test_exp res expected =
    match res ,expected with
    |[a;b],[c;d] -> if(sexpr_eq a c) then test_exp [b] [d] else false
    |[a],[c] -> if(sexpr_eq a c) then true else false
  

(* Failed cases 57 59 62 72 *)



   let test21 = test_exp (read_sexprs("1e1")) [Number (Float 10.)];; 
 let test26 = test_exp (read_sexprs("(    #;#t    )")) [Nil];; 
  let test33 = test_exp (read_sexprs("(a b . c)")) [Pair (Symbol "a", Pair (Symbol "b", Symbol "c"))];;

  (* problems in sexpr_comments  *)
  (* let test57 = test_exp (read_sexprs("(#; 1 (2 #; 3) #; (1 2) 3 #; #; (1 #; 3) 2 3)")) [Pair (Pair (Number (Fraction (2, 1), Nil),Pair (Number (Fraction (3, 1)), Pair (Number (Fraction (3, 1)), Nil))))];; *)  
  (* let test59 = test_exp (read_sexprs("`(1 ;asd\n 2 3 #;#;#;123 2 3)  ")) [Pair (Symbol "quasiquote",Pair(Pair (Number (Fraction (1, 1)), Pair (Number (Fraction (2, 1)), Pair (Number (Fraction (3, 1)), Nil))),Nil))];;  *)
  (* let test62 = test_exp (read_sexprs("(a . (b #;#t . ( (c . d) . e)))  ")) [Pair (Symbol "a",Pair (Symbol "b", Pair (Pair (Symbol "c", Symbol "d"), Symbol "e")))];;  *)
  let test64 = test_exp (read_sexprs("(a b 1 (c . d) e . ())  ")) [Pair (Symbol "a",Pair (Symbol "b",Pair (Number (Fraction (1, 1)),Pair (Pair (Symbol "c", Symbol "d"), Pair (Symbol "e", Nil)))))];;
  let test65 = test_exp (read_sexprs("(a b 1 (() . d) e . ())  ")) [Pair (Symbol "a",Pair (Symbol "b",Pair (Number (Fraction (1, 1)),Pair (Pair (Nil, Symbol "d"), Pair (Symbol "e", Nil)))))];;
  let test94 = test_exp (read_sexprs("'(#\\P 2020.2 2020 \"COVID19\" . #t)  ")) [Pair(Symbol("quote"),Pair(Pair(Char('P'), Pair(Number(Float(2020.2)), Pair(Number(Fraction (2020, 1)), Pair(String("COVID19"), Bool(true))))),Nil))];;
  let test96 = test_exp (read_sexprs(",(#\\F 121212.212121 121212 \"the\" . #t)  ")) [Pair(Symbol("unquote"),Pair(Pair(Char('F'), Pair(Number(Float(121212.212121)), Pair(Number(Fraction (121212, 1)), Pair(String("the"), Bool(true))))),Nil))];;
  let test98 = test_exp (read_sexprs(",!$^*-_=+<>?/:abcdefghijklmnopqrstuvwxyz0123456787  ")) [Pair(Symbol("unquote"),Pair(Symbol("!$^*-_=+<>?/:abcdefghijklmnopqrstuvwxyz0123456787"),Nil))];;
  let test99 = test_exp (read_sexprs("`(#\\D 1234.1234 1234 \"OK\" . #T)  ")) [Pair(Symbol("quasiquote"),Pair(Pair(Char('D'), Pair(Number(Float(1234.1234)), Pair(Number(Fraction (1234, 1)), Pair(String("OK"), Bool(true))))),Nil))];;
  let test109 = test_exp (read_sexprs("`(#\\c 555.555 555.555 \"test\" . #f)")) [Pair(Symbol("quasiquote"),Pair(Pair(Char('c'), Pair(Number(Float(555.555)), Pair(Number(Float(555.555)), Pair(String("test"), Bool(false))))),Nil))];;
  
  (* problem in the dot list *)
  let test122 = test_exp (read_sexprs("(#\\A 123456.234 555 \"test\" . ;blablabla\n #t)")) [Pair(Char('A'), Pair(Number(Float(123456.234)), Pair(Number(Fraction (555, 1)), Pair(String("test"), Bool(true)))))];;
  let test133 = test_exp (read_sexprs("( #\\c 37392.39382 37392 ;fsdfds#$#$#%$#\n . \"that\" )  ")) [Pair(Char('c'), Pair(Number(Float(37392.39382)), Pair(Number(Fraction(37392, 1)), String("that"))))];;
  let test134 = test_exp (read_sexprs("( #\\c 37392.39382 . 37393 )  ")) [Pair(Char('c'), Pair(Number(Float(37392.39382)), Number(Fraction(37393, 1))))];;
  let test132 = test_exp (read_sexprs(",@( #\\c 3/4 4/5 \"this\" )")) [Pair(Symbol("unquote-splicing"),Pair(Pair(Char('c'), Pair(Number(Fraction (3, 4)), Pair(Number(Fraction (4, 5)), Pair(String("this"), Nil)))),Nil))];;
  let test131  = test_exp (read_sexprs(",@3/15")) [Pair(Symbol("unquote-splicing"),Pair(Number(Fraction (1, 5)),Nil))];;
  let test130 = test_exp (read_sexprs(",@\"String with unquote splicing\"")) [Pair(Symbol("unquote-splicing"),Pair(String("String with unquote splicing"),Nil))];;
  let test129 = test_exp (read_sexprs(",@(#T . 1/3)")) [Pair(Symbol("unquote-splicing"),Pair(Pair(Bool(true),Number (Fraction (1, 3))),Nil))];;
  let test128 = test_exp (read_sexprs("-30/300")) [Number (Fraction (-1, 10))];;
  let test127 = test_exp (read_sexprs("18/21")) [Number (Fraction (6, 7))];;
  let test126 = test_exp (read_sexprs("3/9")) [Number (Fraction (1, 3))];;
  let test125 = test_exp (read_sexprs("-3/4")) [Number (Fraction (-3, 4))];;
  let test124 = test_exp (read_sexprs("2/4")) [Number (Fraction (1, 2))];;
  let test123 = test_exp (read_sexprs("1/7")) [Number (Fraction (1, 7))];;
  let test121 = test_exp (read_sexprs("(#\\newline #\\page #\\return #\\space #\\tab #\\newLINe #\\paGE #\\retURN #\\spACE #\\TaB)")) [Pair (Char '\n', Pair (Char '\012', Pair (Char '\r', Pair (Char ' ', Pair (Char '\t', Pair (Char '\n', Pair (Char '\012', Pair (Char '\r', Pair (Char ' ', Pair (Char '\t', Nil))))))))))];;
  let test120 = test_exp (read_sexprs("4")) [Number (Fraction (4, 1))];;
  let test119 = test_exp (read_sexprs("9")) [Number (Fraction (9, 1))];;
  let test118 = test_exp (read_sexprs("( \"test pair\" )")) [Pair(String("test pair"), Nil)];;
  let test117 = test_exp (read_sexprs("( #\\a 555.555 )")) [Pair(Char('a'), Pair(Number(Float(555.555)), Nil))];;
  let test116 = test_exp (read_sexprs("#\\K")) [Char 'K'];;
  let test115 = test_exp (read_sexprs("#\\D")) [Char 'D'];;
  let test114 = test_exp (read_sexprs("#\\+")) [Char '+'];;
  let test113 = test_exp (read_sexprs(",@(#f . ababab)")) [Pair(Symbol("unquote-splicing"),Pair(Pair(Bool(false),Symbol("ababab")),Nil))];;
  let test112 = test_exp (read_sexprs(",@abcdefgh")) [Pair(Symbol("unquote-splicing"),Pair(Symbol("abcdefgh"),Nil))];;
  let test111 = test_exp (read_sexprs(",@\"This is a string with unquote splicing \n \"")) [Pair(Symbol("unquote-splicing"),Pair(String("This is a string with unquote splicing \n "),Nil))];;
  let test110 = test_exp (read_sexprs(",@4")) [Pair(Symbol("unquote-splicing"),Pair(Number(Fraction (4, 1)),Nil))];;
  let test109 = test_exp (read_sexprs("`(#\\c 555.555 555.555 \"test\" . #f)")) [Pair(Symbol("quasiquote"),Pair(Pair(Char('c'), Pair(Number(Float(555.555)), Pair(Number(Float(555.555)), Pair(String("test"), Bool(false))))),Nil))];;
  let test108 = test_exp (read_sexprs("`#\\tab")) [Pair(Symbol("quasiquote"),Pair(Char('\t'),Nil))];;
  let test107 = test_exp (read_sexprs("`#\\newline")) [Pair(Symbol("quasiquote"),Pair(Char('\n'),Nil))];;
  let test106 = test_exp (read_sexprs(",(#t . #f)")) [Pair(Symbol("unquote"),Pair(Pair(Bool(true),Bool(false)),Nil))];;
  let test105 = test_exp (read_sexprs(",( #\\c . 303030 )")) [Pair(Symbol("unquote"),Pair(Pair(Char('c'), Number(Fraction(303030, 1))),Nil))];;
  let test104 = test_exp (read_sexprs(",\"This is a string with \n \"")) [Pair(Symbol("unquote"),Pair(String("This is a string with \n "),Nil))];;
  let test103 = test_exp (read_sexprs("'()")) [Pair(Symbol("quote"),Pair(Nil,Nil))];;
  let test102 = test_exp (read_sexprs("'(#t . #f)")) [Pair(Symbol("quote"),Pair(Pair(Bool(true),Bool(false)),Nil))];;
  let test101 = test_exp (read_sexprs("'( 37392 )")) [Pair(Symbol("quote"),Pair(Pair(Number(Fraction (37392, 1)),Nil),Nil))];;
  let test100 = test_exp (read_sexprs("'10.99")) [Pair(Symbol("quote"),Pair(Number(Float(10.99)),Nil))];;
  let test99 = test_exp (read_sexprs("`(#\\D 1234.1234 1234 \"OK\" . #T)  ")) [Pair(Symbol("quasiquote"),Pair(Pair(Char('D'), Pair(Number(Float(1234.1234)), Pair(Number(Fraction (1234, 1)), Pair(String("OK"), Bool(true))))),Nil))];;
  let test98 = test_exp (read_sexprs(",!$^*-_=+<>?/:abcdefghijklmnopqrstuvwxyz0123456787")) [Pair(Symbol("unquote"),Pair(Symbol("!$^*-_=+<>?/:abcdefghijklmnopqrstuvwxyz0123456787"),Nil))];;
  let test97 = test_exp (read_sexprs(",(#\\D 23.32 12 \"okokok\" #t)  ")) [Pair(Symbol("unquote"),Pair(Pair(Char('D'), Pair(Number(Float(23.32)), Pair(Number(Fraction (12, 1)), Pair(String("okokok"), Pair(Bool(true), Nil))))),Nil))];;
  let test96 = test_exp (read_sexprs(",(#\\F 121212.212121 121212 \"the\" . #t)")) [Pair(Symbol("unquote"),Pair(Pair(Char('F'), Pair(Number(Float(121212.212121)), Pair(Number(Fraction (121212, 1)), Pair(String("the"), Bool(true))))),Nil))];;
  let test95 = test_exp (read_sexprs("'(#\\O 101010.01 37392 \"comp\" #t)")) [Pair(Symbol("quote"),Pair(Pair(Char('O'), Pair(Number(Float(101010.01)), Pair(Number(Fraction (37392, 1)), Pair(String("comp"), Pair(Bool(true), Nil))))),Nil))];;
  let test94 = test_exp (read_sexprs("'(#\\P 2020.2 2020 \"COVID19\" . #t)")) [Pair(Symbol("quote"),Pair(Pair(Char('P'), Pair(Number(Float(2020.2)), Pair(Number(Fraction (2020, 1)), Pair(String("COVID19"), Bool(true))))),Nil))];;
  let test93 = test_exp (read_sexprs("\"\n\"")) [String "\n"];;
  let test92 = test_exp (read_sexprs("\"\r\"")) [String "\r"];;
  let test91 = test_exp (read_sexprs("dag")) [Symbol "dag"];;
  let test90 = test_exp (read_sexprs("DaG  ")) [Symbol "dag"];;
  let test89 = test_exp (read_sexprs("ABC")) [Symbol "abc"];;
  let test88 = test_exp (read_sexprs("aBc")) [Symbol "abc"];;
  let test87 = test_exp (read_sexprs("#\\k  ")) [Char 'k'];;
  let test86 = test_exp (read_sexprs("#\\c")) [Char 'c'];;
  let test85 = test_exp (read_sexprs("#\\d")) [Char 'd'];;
  let test84 = test_exp (read_sexprs("#\\A")) [Char 'A'];;
  let test83 = test_exp (read_sexprs(";asdi39isksdkmkdsf\n #t")) [Bool true];;
  let test82 = test_exp (read_sexprs("#f;sadasujnxjzcn ij49")) [Bool false];;
  let test81 = test_exp (read_sexprs("; sdfdker 4594359 fdskfs\n#t  ")) [Bool true];;
  let test80 = test_exp (read_sexprs("7")) [Number (Fraction (7, 1))];;
  let test79 = test_exp (read_sexprs("(    )")) [Nil];;
  let test78 = test_exp (read_sexprs("()")) [Nil];;
  let test76 = test_exp (read_sexprs("5.6")) [Number (Float 5.6)];;
  let test75 = test_exp (read_sexprs("2.3")) [Number (Float 2.3)];;
  let test74 = test_exp (read_sexprs("1.3")) [Number (Float 1.3)];;
  let test72 = test_exp (read_sexprs("\"This is a very long
  string that spills across
  several lines.\"")) [String "This is a very long\nstring that spills across\nseveral lines."];;
  let test68 = test_exp (read_sexprs("1.23e+1")) [Number (Float 12.3)];;
  let test67 = test_exp (read_sexprs("123456789e-9")) [Number (Float 0.123456789)];;
  let test66 = test_exp (read_sexprs("(().())")) [Pair (Nil, Nil)];;
  let test65 = test_exp (read_sexprs("(a b 1 (() . d) e . ())")) [Pair (Symbol "a",Pair (Symbol "b",Pair (Number (Fraction (1, 1)),Pair (Pair (Nil, Symbol "d"), Pair (Symbol "e", Nil)))))];;
  let test64 = test_exp (read_sexprs("(a b 1 (c . d) e . ())")) [Pair (Symbol "a",Pair (Symbol "b",Pair (Number (Fraction (1, 1)),Pair (Pair (Symbol "c", Symbol "d"), Pair (Symbol "e", Nil)))))];;
  let test63 = test_exp (read_sexprs("((a b c) . (d . ( (e . f) . g)))")) [Pair (Pair (Symbol "a", Pair (Symbol "b", Pair (Symbol "c", Nil))),Pair (Symbol "d", Pair (Pair (Symbol "e", Symbol "f"), Symbol "g")))];;
  

let bool1 = test_exp (read_sexprs(" #t  ")) ([Bool(true)]);;





