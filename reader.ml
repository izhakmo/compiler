
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
  val nt_whitespaces : char list -> char list * char list
  val make_paired : ('a -> 'b * 'c) -> ('d -> 'e * 'f) -> ('c -> 'g * 'd) -> 'a -> 'g * 'f
  val make_spaced : (char list -> 'a * char list) -> char list -> 'a * char list
  val boolOrBackSlash : char -> sexpr
  val nt_boolean : char list -> sexpr * char list
  val digit : char list -> char * char list
  val tok_num_ : char list -> number * char list


(*)  val boolOrBackSlash : char -> sexpr*)
end
= struct
let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (lowercase_ascii ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;


(* if we find # ==> we need to check for a boolean insensative or char*)

let hash = (char '#');;
let t = (char 't');;
let f = (char 'f');;


let boolOrBackSlash x  = match x with
  | 'f'-> Bool(false)
  | 'F'-> Bool(false)
  | 't'-> Bool(true)
  | 'T'-> Bool(true)
  | _ -> raise X_not_yet_implemented;;

let nt_boolean = 
  let bool_token = caten hash (disj (char_ci 't') (char_ci 'f')) in 
  pack bool_token (fun (hash, t_or_f) -> (boolOrBackSlash t_or_f));;

(*let nt_boolean = caten hash boolOrBackSlash;;*)

let digit = range '0' '9';; 

let tok_num_ =
  let digits = plus digit in
  pack digits (fun (ds) -> Fraction (int_of_string (list_to_string ds), 1));;

(* TODO ADD SIGN plux or minus before*)



let _tokenize_num x = match x with
  | '/' -> String("TODO FRAC and denominator")
  | '.' -> String("TODO float")
  | _ -> String("TODO FRAC divided by 1");;

let dot = '.';; 


let lowerCase = range 'a' 'z';;
let upperCase = range 'A' 'Z';;
(* let punctuation = 
  | '!' | '$' | '^' | '*' | '-' | '_' 
  | '=' | '+' | '<' | '>' | '/' | '?';; *)

(* let dots = '.' | ',';; *)

(*let _symbolToken = *)

(* let string_meta_char = '\' | '"' | 't' | 'f' | 'n' | 'r'; *)

let _tokenize_meta_char x = match x with 
  | '\\' -> Char('\\') 
  | '\"' -> Char('\"')
  | '\t' -> Char('\t')
  | '\012' -> Char('\012')   (*\f*)
  | '\n' -> Char('\n')
  | '\r' -> Char('\r')
  | _ -> raise X_not_yet_implemented;;

(*
let _tokenize_named_char x = match x with
  | "#\nul" -> Char('\000') 
  | "#\newline" -> Char('\010') 
  | "#\return" -> Char('\013') 
  | "#\tab" -> Char('\009') 
  | "#\formfeed" -> Char('\012') 
  | "#\space" -> Char('\032');;
  | _ -> (_tokenize_visible_char x);;


let _tokenize_visible_char c = match x with
  | Char(c) && (c > '\032') -> Char(x)
  | _ -> raise X_not_yet_implemented;;
*)




let make_paired nt_left nt_right nt =
let nt = caten nt_left nt in
let nt = pack nt (function (_, e) -> e) in
let nt = caten nt nt_right in
let nt = pack nt (function (e, _) -> e) in
nt;;

let nt_whitespaces = star nt_whitespace;;

let make_spaced nt =
make_paired nt_whitespaces nt_whitespaces nt;;

let tok_lparen = make_spaced ( char '(');;

let tok_rparen = make_spaced ( char ')');;



let read_sexprs string = raise X_not_yet_implemented;;


 
end;; (* struct Reader *)
open Reader;; 

(* test_string nt_boolean "#T";; *)
