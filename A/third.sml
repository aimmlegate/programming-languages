(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		             | Variable of string
		             | UnitP
		             | ConstP of int
		             | TupleP of pattern list
		             | ConstructorP of string * pattern

datatype valu = Const of int
	            | Unit
	            | Tuple of valu list
	            | Constructor of string * valu

fun g f1 f2 p =
    let
	      val r = g f1 f2
    in
	      case p of
	          Wildcard          => f1 ()
	        | Variable x        => f2 x
	        | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	        | ConstructorP(_,p) => r p
	        | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	           | UnitT
	           | IntT
	           | TupleT of typ list
	           | Datatype of string

                               (**** you can put all your code here ****)

(*1*)

fun only_capitals strings =
    List.filter (fn el => Char.isUpper (String.sub(el, 0))) strings

(*2*)

fun longest_string1 strings =
    List.foldl (fn (string, init) =>
                   if String.size string > String.size init
                   then string
                   else init
               ) "" strings

(*3*)

fun longest_string2 strings =
    List.foldl (fn (string, init) =>
                   if String.size string >= String.size init
                   then string
                   else init
               ) "" strings

(*4*)

fun longest_string_helper pr strings =
    List.foldl (fn (string, init) =>
                   if pr (String.size string, String.size init)
                   then string
                   else init
               ) "" strings

val longest_string3 = longest_string_helper (fn (s, i) => s > i)

val longest_string4 = longest_string_helper (fn (s, i) => s >= i)

(*5*)

val longest_capitalized = longest_string3 o only_capitals

(*6*)

val rev_string = String.implode o List.rev o String.explode

(*7*)

fun first_answer f list =
    case list of
       [] => raise NoAnswer
     | hd::tl => let val answer = f hd in
                     if isSome answer then valOf answer
                     else first_answer f tl
                 end

fun all_answers f list =
    let
        fun aux (lst, acc) =
            case (lst, acc) of
               ([], []) => SOME []
             | ([], acc) => SOME acc
             | (NONE::tl, _) => NONE
             | (SOME(i)::tl, _) => aux(tl, acc @ i)
    in
        aux(List.map f list, [])
    end
 
