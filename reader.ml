
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
  (* val tok_num : char list -> number * char list *)
  val natural : char list -> char list * char list
  val sign : char list -> char * char list

  val gen_integer : char option * char list -> string
  val nt_integer : char list -> string * char list
  
  val gen_float : string * (char * char list) -> sexpr 
  val nt_float : char list -> sexpr * char list

  val gen_fraction : string * (char * char list) -> sexpr 
  val nt_fraction : char list -> sexpr * char list

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
  val string_char_plus : char list -> sexpr * char list 
  

  val namedChar_match : string -> char 
  val namedChar : char list -> char * char list 
  val string_meta_char : char list -> char * char list 
  val visibleSimpleChar : char list -> char * char list 
  val prefixed_char : char list -> char list * char list 
  val nt_char : char list -> sexpr * char list 


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
  let bool_token = make_spaced (caten hash (disj (char_ci 't') (char_ci 'f'))) in 
  pack bool_token (fun (hash, t_or_f) -> (boolOrBackSlash t_or_f));;



let digit = range '0' '9';; 


let natural = plus digit;;



let gen_integer (l, tl) = match l with
  | Some('+') ->  (list_to_string tl)
  | Some('-') ->  (String.concat "" ["-";(list_to_string tl)])
  | None  ->  (list_to_string tl)
  | _ -> raise X_no_match;;


let nt_integer = pack (caten (maybe sign) natural) gen_integer;; 



let gen_float (l ,(p , tl)) = match p with 
    | '.' -> Number(Float(float_of_string (String.concat "" [ l;".";(list_to_string tl)])))
    | _ -> raise X_no_match;;



let nt_float = (pack (caten nt_integer (caten dot natural)) gen_float);;


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

let nt_int_integer = (pack nt_integer (fun (int_moshe) -> Number(Fraction(int_of_string int_moshe, 1))));;

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

let string_char_plus = (pack (plus string_char) (fun (hd)-> String(list_to_string hd)));;


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

(* nuL ascii *)




let tok_lparen = make_spaced ( char '(');;

let tok_rparen = make_spaced ( char ')');;



let read_sexprs string = raise X_not_yet_implemented;;


 
end;; (* struct Reader *)
open Reader;; 

(* test_string nt_boolean "#T";; *)
