(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (sample: string, list: string list) =
    let
        fun check (lst) =
            case lst of
                [] => false
              | x::xs' => same_string(x, sample) orelse check(xs')

        fun filter (lst) =
            case lst of
                [] => []
              | x::xs' => if same_string(x, sample)
                          then filter(xs')
                          else x :: filter(xs')
    in
        if check list
        then SOME (filter list)
        else NONE
    end

fun get_substitutions1 (list, sample) =
    case list of
        [] => []
      | x::xs' => case all_except_option(sample, x) of
                      NONE => [] @ get_substitutions1(xs', sample)
                    | SOME i => i @ get_substitutions1(xs', sample)

fun get_substitutions2 (list, sample) =
    let
        fun aux (lst, acc) =
            case lst of
                [] => acc
              | x::xs' => case all_except_option(sample, x) of
                              NONE => aux(xs', acc)
                            | SOME i => aux(xs', acc @ i)
    in
        aux(list, [])
    end

fun similar_names (list, record) =
    let
        fun aux (lst, acc) =
            case (lst, record) of
                ([], _) => record::acc
              | (x::xs', {first=_, last=l, middle=m}) => aux(xs', {first=x, last=l, middle=m}::acc)
        val {first=f, middle=_, last=_} = record
        val subst = get_substitutions2(list, f)
    in
        aux(subst, [])
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove


(* put your solutions for problem 2 here *)
