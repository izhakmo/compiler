
#use "pc.ml";;
open PC;;

exception X_not_yet_implemented;;
exception X_this_should_not_happen;;
  
type number =
  | Fraction of int * int
  | Float of float;;
  
type sexpr =
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr;;

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

module Reader: sig
  val read_sexprs : string -> sexpr list

  
  val hash : char list -> char * char list
  val dot : char list -> char * char list
  val slash : char list -> char * char list
  val nt_whitespaces : char list -> char list * char list
  val make_paired : ('a -> 'b * 'c) -> ('d -> 'e * 'f) -> ('c -> 'g * 'd) -> 'a -> 'g * 'f
  val make_spaced : (char list -> 'a * char list) -> char list -> 'a * char list
  val boolOrBackSlash : char -> sexpr
  val nt_boolean : char list -> sexpr * char list
  val digit : char list -> char * char list

  val nt_e : char list -> char * char list
  (* val tok_num : char list -> number * char list *)
  val natural : char list -> char list * char list
  val sign : char list -> char * char list

  val gen_integer : char option * char list -> string
  val nt_integer : char list -> string * char list

  val nt_e_exponent : char list -> (char * string) option * char list
  
  val gen_float : string * ('a * (char list * ('b * string) option)) -> sexpr 
  val nt_float : char list -> sexpr * char list

  val gen_fraction : string * (char * char list) -> sexpr 
  val nt_fraction : char list -> sexpr * char list

  val gen_integer_or_float_undotted : string * ('a * string) option -> sexpr

  val nt_int_integer : char list -> sexpr * char list
  val nt_number : char list -> sexpr * char list 

  val lowerCase : char list -> char * char list 
  val upperCase : char list -> char * char list 
  val nt_SymbolCharNoDot : char list -> char * char list 
  val nt_SymbolChar : char list -> char * char list 

  val nt_Symbol : char list -> sexpr * char list 

  
  val string_meta_char_match : string -> char 
  val string_meta_char : char list -> char * char list 
  val stringLiteralChar : char list -> char * char list 
  val string_char : char list -> char * char list 
  (* val string_char_plus : char list -> sexpr * char list  *)
  val string_char_plus : char list -> char list * char list 
  val double_qoute : char list -> char * char list
  val nt_string : char list -> sexpr * char list 

  val namedChar_match : string -> char 
  val namedChar : char list -> char * char list 
  val string_meta_char : char list -> char * char list 
  val visibleSimpleChar : char list -> char * char list 
  val prefixed_char : char list -> char list * char list 
  val nt_char : char list -> sexpr * char list 


  val tok_lparen : char list -> char * char list 
  val tok_rparen : char list -> char * char list 
  val nt_nil : char list -> sexpr * char list 
  
  val nt_sexper_not_pair : char list -> sexpr * char list
  val nt_sexper_plural : char list -> sexpr list * char list
  (* val nt_proper_list : char list -> sexpr * char list  *)
  (* val nt_pair : char list -> sexpr * char list  *)
  (* val nt_sexper_plus : char list -> sexpr list * char list *)
  val nt_pair : char list -> sexpr * char list 
  val nt_list_proper : char list -> sexpr * char list 
  val nt_list_improper : char list -> sexpr * char list 
  val _sexpr : char list -> sexpr * char list 
  
  val quote_match : string -> string 
  val quotes : char list -> sexpr * char list 


end
= struct
let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (lowercase_ascii ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;




let hash = (char '#');;
let t = (char 't');;
let f = (char 'f');;
let sign = disj (char '+') (char '-');;
let dot = (char '.');;
let slash = (char '/');;

let boolOrBackSlash x  = match x with
  | 'f'-> Bool(false)
  | 'F'-> Bool(false)
  | 't'-> Bool(true)
  | 'T'-> Bool(true)
  | _ -> raise X_not_yet_implemented;;


let make_paired nt_left nt_right nt =
  let nt = caten nt_left nt in
  let nt = pack nt (function (_, e) -> e) in
  let nt = caten nt nt_right in
  let nt = pack nt (function (e, _) -> e) in
  nt;;

let nt_whitespaces = star nt_whitespace;;


let make_spaced nt =
  make_paired nt_whitespaces nt_whitespaces nt;;

let nt_boolean = 
  let bool_token = (caten hash (disj (char_ci 't') (char_ci 'f'))) in 
  pack bool_token (fun (hash, t_or_f) -> (boolOrBackSlash t_or_f));;



let digit = range '0' '9';; 

let nt_e = (char_ci 'e');;


let natural = plus digit;;



let gen_integer (l, tl) = match l with
  | Some('+') ->  (list_to_string tl)
  | Some('-') ->  (String.concat "" ["-";(list_to_string tl)])
  | None  ->  (list_to_string tl)
  | _ -> raise X_no_match;;


let nt_integer = pack (caten (maybe sign) natural) gen_integer;; 

let nt_e_exponent = maybe (caten nt_e nt_integer);;


let gen_float (hd ,(p ,(tl,exp ))) = match exp with 
    | Some(_,x) -> Number(Float((10.0 ** (float_of_string(x)) *.
                        float_of_string (String.concat "" [ hd;".";(list_to_string tl)]))))
    | None -> Number(Float(float_of_string (String.concat "" [ hd;".";(list_to_string tl)])));;


(* let nt_float = (pack (caten nt_integer (caten dot natural)) gen_float);; *)

let nt_float = (pack (caten nt_integer (caten dot (caten natural nt_e_exponent))) gen_float) ;;


let gen_fraction (l ,(p , tl)) =
  let numerator = (int_of_string l) in
  let denominator = (int_of_string (list_to_string tl)) in
  let rec gcd a b = 
    if (a == 0)
      then b
      else gcd (b mod a) a
  in
  let ans = (gcd numerator denominator) in 
  let ans_without_sign = 
    if (ans > 0) then ans else ans * (-1) in
  Number(Fraction(numerator/ ans_without_sign, denominator/ans_without_sign));;

    

let nt_fraction = (pack (caten nt_integer (caten slash natural)) gen_fraction);;


let gen_integer_or_float_undotted (hd, tl) = match tl with
    | Some(_,x) -> Number(Float((10.0 ** (float_of_string(x)) *.  (float_of_string hd ))))
    | None -> Number(Fraction(int_of_string hd, 1));;

let nt_int_integer = (pack (caten nt_integer nt_e_exponent) gen_integer_or_float_undotted);;

let nt_number = disj nt_float (disj nt_fraction nt_int_integer);;




let lowerCase = range 'a' 'z';;
let upperCase = range 'A' 'Z';;
 
let nt_SymbolCharNoDot = disj_list [digit; lowerCase; (pack upperCase lowercase_ascii); make_one_of char "!$^*-_=+<>/?:"];;

let nt_SymbolChar = disj nt_SymbolCharNoDot dot;;

let nt_Symbol = (pack
                      (disj (pack (caten nt_SymbolChar (plus nt_SymbolChar)) (fun (e, es) -> (e :: es)))
                            (pack (caten nt_SymbolCharNoDot nt_epsilon) (fun (e, es) -> (e :: es))))
                      (fun (hd)-> Symbol(list_to_string hd)));;


let string_meta_char_match str = match str with
  | "\\\\" -> '\092'
  | "\\\"" -> '\034'
  | "\\t" -> '\009'
  | "\\f"  -> '\012'
  | "\\n"  -> '\010'
  | "\\r"  -> '\013'
  | _ -> raise X_no_match;;

(* This code parse the ocaml language and not scheme language *)
let string_meta_char = (pack (disj_list [word_ci "\\\\"; word_ci "\\\""; word_ci "\\t"; word_ci "\\f"; word_ci "\\n"; word_ci "\\r"]) 
                             (fun (hd)-> (string_meta_char_match (list_to_string hd))));;


let stringLiteralChar = disj_list [(range '\032' '\033'); (range '\035' '\091');(range '\093' '\126')];;

let string_char = disj stringLiteralChar string_meta_char;;

let string_char_plus = plus string_char;;

let double_qoute = (char_ci '\"');;

let nt_string = (pack  (make_paired double_qoute double_qoute string_char_plus)   (fun (hd)-> String(list_to_string hd)));;




(* test_string nt_sexper "  \\\"tab\\\"   ";;


test_string nt_sexper "\\\"  \\\"" *)



(* 
let namedChar_match x = match x with
  | "nul"       -> Char('\000') 
  | "newline"   -> Char('\010') 
  | "return"    -> Char('\013') 
  | "tab"       -> Char('\009') 
  | "formfeed"  -> Char('\012') 
  | "space"     -> Char('\032')
  | _           -> raise X_no_match;; *)



let namedChar_match str = match str with
  | "nul"       -> '\000'
  | "newline"   -> '\010'
  | "return"    -> '\013'
  | "tab"       -> '\009'
  | "formfeed"  -> '\012'
  | "space"     -> '\032'
  | _           -> raise X_no_match;;
  



(* This code parse the ocaml language and not scheme language *)
let namedChar = (pack (disj_list [word_ci "nul"; word_ci "newline"; word_ci "return"; word_ci "tab"; word_ci "formfeed"; word_ci "space"]) 
                             (fun (hd)-> (namedChar_match (list_to_string (List.map lowercase_ascii hd)))));;


let visibleSimpleChar = range '\032' '\126';;

let prefixed_char = (pack (caten hash (char '\\'))  (fun (one, two) -> (one :: [two]))) ;;


let nt_char =  
    let char_token = (caten prefixed_char (disj namedChar visibleSimpleChar)) in 
    pack char_token (fun (prefixed, tokenized) -> (Char (tokenized)));;

(* #;( #; (#; (#; moshe) hagever) q) *)




let tok_lparen = make_spaced ( char '(');;

let tok_rparen = make_spaced ( char ')');;

let nt_nil = (pack (make_paired tok_lparen tok_rparen nt_epsilon) (fun (_)-> Nil ));;
(* comments inside instead of epsilon *)
(* These may enclose whitespaces and comments *)



let nt_sexper_not_pair = make_spaced (disj_list [nt_boolean ; nt_number ; nt_Symbol; nt_string; nt_char ; nt_nil]);;

let nt_sexper_plural = plus nt_sexper_not_pair;;


(* ((1 2) . 3) ->Pair (Pair (Number (Int 1), Pair (Number (Int 2), Nil)), Number (Int 3))

(* 1.(a b (c d)) *)

1.1 (a) -> Pair(Symbol(a), Nil)

2.(a b c) -> Pair(a, Pair(b,Pair(c ,Nil)))

3. (a b . c) -> Pair(a, Pair(b,c)) *)

(* 4.(a . (b c)) -> Pair(a, Pair(b,Pair (c, Nil))) *)

(* (#T moshe . 3.14 ) -> Pair(Bool (true), 
                        Pair(Symbol("moshe"),Float(3.14) ) 
                        )


(* proper list is nested pair that ends with nil  *)

(#T moshe 3.14 ) -> Pair(Bool (true), 
                        Pair(Symbol("moshe"),
                             Pair(Float(3.14), Nil ) 
                            )
                        )
  
 *)


let rec _sexpr lst= (disj_list [nt_pair; nt_sexper_not_pair; nt_list_proper; nt_list_improper]) lst

and nt_pair lst=
  let nt_dot = caten tok_lparen (caten _sexpr (caten dot (caten _sexpr tok_rparen))) in 
              pack nt_dot (fun (lp, (car, (dot, (cdr, rp)))) -> Pair(car, cdr)) lst

and nt_list_proper lst= 
  let nt_proper_list = caten tok_lparen (caten (star _sexpr) tok_rparen) in 
                                (pack nt_proper_list (fun (lp, (hd::tl, rp)) -> (List.fold_right (fun e aggr -> Pair(e, aggr)) (hd::tl) Nil ))) lst


and nt_list_improper lst= 
let nt_improper_list = caten tok_lparen (caten (plus _sexpr) (caten dot (caten _sexpr tok_rparen))) in 
                              (pack nt_improper_list (fun (lp, (hd::tl, (dot, (cdr, rp)))) -> 
                                                    (List.fold_right (fun e aggr -> Pair(e, aggr)) (hd::tl) cdr ))) lst;;


(* 
let quote = pack (caten (char '\'') _sexpr) (fun (tok_quote, exper)-> Pair( Symbol("quote") , Pair(exper, Nil)));;

let quasi_quote = pack (caten (char '`') _sexpr) (fun (tok_quote, exper)-> Pair( Symbol("quasiquote") , Pair(exper, Nil)));; *)

(* let quote = pack (caten (char '\'') _sexpr) (fun (tok_quote, exper)-> Pair( Symbol("quote") , Pair(exper, Nil)));; *)

let quote_match str = match str with
  | "\\'"       -> "quote"
  | "`"   -> "quasiquote"
  | ",@"       -> "unquote-splicing"
  | ","       -> "unquote"
  | _           -> raise X_no_match;;

let quotes = (pack (caten (disj_list [(word "\\'"); (word "`"); (word ",@"); (word ",")]) _sexpr) 
                            (fun (tok_quote, exper)-> Pair( Symbol((quote_match (list_to_string tok_quote))) , Pair(exper, Nil))));;


let read_sexprs string = raise X_not_yet_implemented;;


 
end;; (* struct Reader *)
open Reader;; 

(* test_string nt_boolean "#T";; *)
