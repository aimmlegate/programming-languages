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
                   if String.size string > String.size init then string else init) "" strings

(*3*)

fun longest_string2 strings =
    List.foldl (fn (string, init) =>
                   if String.size string >= String.size init then string else init) "" strings

(*4*)

fun longest_string_helper pr strings =
    List.foldl (fn (string, init) =>
                   if pr (String.size string, String.size init) then string else init) "" strings

val longest_string3 = longest_string_helper (fn (s, i) => s > i)

val longest_string4 = longest_string_helper (fn (s, i) => s >= i)

(*5*)

val longest_capitalized = longest_string3 o only_capitals

(*6*)

val rev_string = String.implode o List.rev o String.explode

(*7*)

fun first_answer f list =
    case list of
       []     => raise NoAnswer
     | hd::tl =>
                let
                   val answer = f hd
                in
                   if isSome answer then valOf answer else first_answer f tl
                end

(*8*)

fun all_answers f list =
    let
        fun aux (lst, acc) =
            case (lst, acc) of
               ([], [])         => SOME []
             | ([], acc)        => SOME acc
             | (NONE::tl, _)    => NONE
             | (SOME(i)::tl, _) => aux(tl, acc @ i)
    in
        aux(List.map f list, [])
    end

(*9*)

val count_wildcards = g (fn () => 1) (fn (_) => 0)

val count_wild_and_variable_lengths = g (fn () => 1) (fn (s) => String.size s)

fun count_some_var (string, pattern) =
    g (fn () => 0) (fn (s) => if s = string then 1 else 0) pattern

(*10*)

fun check_pat pattern =
    let
        fun concat pat =
            case pat of
               Variable x => [x]
             | TupleP x   => List.foldl (fn (p, init) => init @ concat p) [] x
             | _          => []
        fun is_set list =
            case list of
               []     => true
             | x::xs' => if List.exists(fn (el) => el = x) xs' then false else is_set xs'
    in
        is_set (concat pattern)
    end

(*11*)

fun match (valu, pattern) =
    case (pattern, valu) of
       (Wildcard, _)         => SOME []
     | (Variable s, v)       => SOME [(s, v)]
     | (UnitP, Unit)         => SOME []
     | (ConstP x, Const y)   => if x = y then SOME [] else NONE
     | (TupleP ps, Tuple vs) =>
                               let
                                  val innerTuple = all_answers match (ListPair.zip(vs, ps))
                               in
                                  if isSome innerTuple then innerTuple else NONE
                               end
     | (ConstructorP(s1,p), Constructor(s2,v)) => if s1 = s2 then match (v, p) else NONE
     | _ => NONE


fun first_match value patterns =
    let
        val m = first_answer (fn p => match(value, p)) patterns
    in
        SOME m
    end
        handle NoAnswer => NONE

