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

fun card_color (card) =
    case card of
        (Clubs, _) => Black
      | (Spades, _)  => Black
      | _ => Red

fun card_value (card) =
    case card of
       (_, Num i) => i
     | (_, Ace) => 10
     | _ => 11

fun remove_card (cs, c, e) =
    let
        fun aux (lst, acc, check) =
            case (lst, acc, check) of
               ([], _, false) => raise e
             | ([], acc, true) => acc
             | (lst, acc, true) => aux([], acc @ lst, true)
             | (x::xs', acc, false) => if x = c
                                      then aux(xs', acc, true)
                                      else aux(xs', x :: acc, false)
    in
        aux(cs, [], false)

    end

fun all_same_color cardlist =
        case cardlist of
            [] => true
          | _::[] => true
          | head::(neck::rest) => (card_color(head) = card_color(neck) andalso all_same_color (neck::rest))

fun sum_cards cardlist =
    let fun aux (lst, acc) =
            case (lst, acc) of
                ([], acc) => acc
              | (x::xs', acc) => aux(xs', acc + card_value(x))
    in
        aux(cardlist, 0)
    end

fun score (cardlist, goal) =
    let
        val summ = sum_cards cardlist
    in
        case (all_same_color cardlist, summ > goal, summ) of
           (false, true, summ) => (summ - goal) * 3
         | (false, false, summ) => goal - summ
         | (true, true, summ) => ((summ - goal) * 3) div 2
         | (true, false, summ) => (goal - summ) div 2
    end

fun officiate (cards, moves, goal) =
    let
        fun aux (cards, hand, moves) =
            case (cards, hand, moves) of
               (_, hand, []) => score (hand, goal)
             | ([], hand, Draw::_) => score (hand, goal)
             | (c::cs', hand, Draw::ms') => let val new_hand = c::hand
                                           in
                                               if sum_cards new_hand > goal
                                               then score (new_hand, goal)
                                               else aux (cs', new_hand, ms')
                                           end
             | (cards, hand, (Discard card)::ms') => aux(cards, remove_card(hand, card, IllegalMove), ms')
    in
        aux (cards, [], moves)
    end


        
