
#use "pc.ml";;

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
end
= struct
let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (lowercase_ascii ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;


(* if we find # ==> we need to check for a boolean insensative or char*))

let _#_ = (char '#');;

let _boolOrBackSlash x = match x with
    | 'f' -> Bool(false)
    | 't' -> Bool(true)
    | _ -> raise X_not_yet_implemented;;

let digit = range '0' '9';; 

(* TODO ADD SIGN plux or minus before*)

let tok_num_=
  let digits= plus digit in 
  pack digits(fun (ds) -> Num (int_of_string(list_to_string ds)));;



let _tokenize_num x = match x with
  | '/' -> String("TODO FRAC and denominator")
  | '.' -> String("TODO float")
  | _ -> String("TODO FRAC divided by 1");;

  let dot = '.';; 


let lowerCase = range 'a' 'z';;
let upperCase = range 'A' 'Z';;
let punctuation = 
  | '!' | '$' | '^' | '*' | '-' | '_' 
  | '=' | '+' | '<' | '>' | '/' | '?';;

let dots = '.' | ',';;

(*let _symbolToken = *)

(* let string_meta_char = '\' | '"' | 't' | 'f' | 'n' | 'r'; *)

let _tokenize_meta_char x = match x with 
  | '\\' -> Char(char '\\') 
  | '\"' -> Char(char '\"')
  | '\t' -> Char(char '\t')
  | '\f' -> Char(char '\f')
  | '\n' -> Char(char '\n')
  | '\r' -> Char(char '\r')
  | _ -> raise X_not_yet_implemented;;

let _tokenize_named_char x = match x with
  | "nul" -> Char(0) 
  | "newline" -> Char(10) 
  | "return" -> Char(13) 
  | "tab" -> Char(9) 
  | "formfeed" -> Char(12) 
  | "space" -> Char(32)
  | _ -> (_tokenize_visible_char x);;

let _tokenize_visible_char x = match x with
  | char(c) and (c > 32) -> Char(char x)
  | _ -> raise X_not_yet_implemented;;


let read_sexprs string = raise X_not_yet_implemented;;
  
end;; (* struct Reader *)
