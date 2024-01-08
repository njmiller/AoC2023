open Core

type camel_card_hand = 
    { a : int;
      k : int;
      q : int;
      j : int;
      ten: int;
      nine: int;
      eight: int;
      seven: int;
      six: int;
      five: int;
      four: int;
      three: int;
      two: int;
      chars: char list;
      order_vals : int list;
      bet: int;
      rank: int;
    }

let check_num hand num = 
    if hand.a = num then true
    else if hand.k = num then true
    else if hand.q = num then true
    else if hand.j = num then true
    else if hand.ten = num then true
    else if hand.nine = num then true
    else if hand.eight = num then true
    else if hand.seven = num then true
    else if hand.six = num then true
    else if hand.five = num then true
    else if hand.four = num then true
    else if hand.three = num then true
    else if hand.two = num then true
    else false

let check_n_pair hand =
    let npair = 0 in
    let npair = npair + hand.a/2 in
    let npair = npair + hand.k/2 in
    let npair = npair + hand.q/2 in
    let npair = npair + hand.j/2 in
    let npair = npair + hand.ten/2 in
    let npair = npair + hand.nine/2 in
    let npair = npair + hand.eight/2 in
    let npair = npair + hand.seven/2 in
    let npair = npair + hand.six/2 in
    let npair = npair + hand.five/2 in
    let npair = npair + hand.four/2 in
    let npair = npair + hand.three/2 in
    npair + hand.two/2

let find_nums hand = 
    let num_joker = hand.j in
    let nums = [hand.a; hand.k; hand.q; hand.ten; hand.nine;
                hand.eight; hand.seven; hand.six; hand.five;
                hand.four; hand.three; hand.two] in
    let nums_sort = List.rev (List.sort ~compare:Int.compare nums) in
    let max :: second :: t = nums_sort in
    (max, second, num_joker)
    
let parse_rank_joker hand = 
    let max, second, num_joker = find_nums hand in
    if max+num_joker = 5 then {hand with rank=6}
    else if max+num_joker = 4 then {hand with rank=5}
    else if max+num_joker = 3 then
        if second = 2 then {hand with rank=4}
        else {hand with rank=3}
    else if max+num_joker = 2 then
        if second = 2 then {hand with rank=2}
        else {hand with rank=1}
    else {hand with rank=0}

let parse_rank hand = 
    if check_num hand 5 then {hand with rank=6}
    else if check_num hand 4 then {hand with rank=5}
    else if check_num hand 3 then
        if check_num hand 2 then {hand with rank=4}
        else {hand with rank=3}
    else {hand with rank=(check_n_pair hand)}

let compare_rank hand1 hand2 = Int.compare hand1.rank hand2.rank

let rec compare_chars_aux tup_list = 
    match tup_list with
    | [] -> 0
    | (a, b) :: t -> let comp = Int.compare a b in
                     if comp = 0 then compare_chars_aux t else comp

let compare_chars hand1 hand2 = 
    let tup_list = List.zip_exn hand1.order_vals hand2.order_vals in
    compare_chars_aux tup_list

let compare_hand hand1 hand2 = 
    let rank_compare = compare_rank hand1 hand2 in
    if rank_compare = 0 then compare_chars hand1 hand2 else rank_compare

let rec parse_hands_aux j_val hand list_chars =
    let parse_hands_aux2 = parse_hands_aux j_val in 
    match list_chars with
    | ' ' :: t -> {hand with bet=(Util.parse_num t); order_vals=(List.rev hand.order_vals)}
    | 'A' :: t -> parse_hands_aux2 {hand with a=hand.a+1; order_vals=14::hand.order_vals} t
    | 'K' :: t -> parse_hands_aux2 {hand with k=hand.k+1; order_vals=13::hand.order_vals} t
    | 'Q' :: t -> parse_hands_aux2 {hand with q=hand.q+1; order_vals=12::hand.order_vals} t
    | 'J' :: t -> parse_hands_aux2 {hand with j=hand.j+1; order_vals=j_val::hand.order_vals} t
    | 'T' :: t -> parse_hands_aux2 {hand with ten=hand.ten+1; order_vals=10::hand.order_vals} t
    | '9' :: t -> parse_hands_aux2 {hand with nine=hand.nine+1; order_vals=9::hand.order_vals} t
    | '8' :: t -> parse_hands_aux2 {hand with eight=hand.eight+1; order_vals=8::hand.order_vals} t
    | '7' :: t -> parse_hands_aux2 {hand with seven=hand.seven+1; order_vals=7::hand.order_vals} t
    | '6' :: t -> parse_hands_aux2 {hand with six = hand.six+1; order_vals=6::hand.order_vals} t
    | '5' :: t -> parse_hands_aux2 {hand with five = hand.five+1; order_vals=5::hand.order_vals} t 
    | '4' :: t -> parse_hands_aux2 {hand with four = hand.four+1; order_vals=4::hand.order_vals} t 
    | '3' :: t -> parse_hands_aux2 {hand with three = hand.three+1; order_vals=3::hand.order_vals} t
    | '2' :: t -> parse_hands_aux2 {hand with two = hand.two+1; order_vals=2::hand.order_vals} t

let parse_hand j_val list_chars = 
    let init_hand = {a=0; k=0; q=0; j=0; ten=0; nine=0;
                     eight=0; seven=0; six=0; five=0;
                     four=0; three=0; two=0;
                     chars=list_chars; rank=0; order_vals=[];
                     bet=0} in
    let hand_out = parse_hands_aux j_val init_hand list_chars in
    let rank_val = parse_rank hand_out in
    (*{hand_out with rank=rank_val}*)
    rank_val

let rec calc_winnings_aux idx hands = 
    match hands with
    | [] -> 0
    | h :: t -> idx*h.bet + calc_winnings_aux (idx+1) t

let calc_winnings hands = calc_winnings_aux 1 hands

(* Set J value to 11 for this part *)
let part1 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let hands = List.map ~f:(parse_hand 11) list_list_chars in
    let hands_sort = List.sort ~compare:compare_hand hands in
    calc_winnings hands_sort

(* Set J value to 1 for this part *)
let part2 filename =
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let hands = List.map ~f:(parse_hand 1) list_list_chars in
    let hands_with_joker = List.map ~f:parse_rank_joker hands in
    let hands_sort = List.sort ~compare:compare_hand hands_with_joker in
    calc_winnings hands_sort
