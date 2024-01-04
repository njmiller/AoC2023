open Core
 
let rec get_num_scratches_aux idx num_scratch num_overlap =
    match num_overlap with
    | [] -> num_scratch
    | h :: t -> let _ = for i = (idx+1) to (idx+h) do
                   num_scratch.(i) <- num_scratch.(i) + num_scratch.(idx)
                done in
                get_num_scratches_aux (idx+1) num_scratch t

let get_num_scratches num_overlap =
    let num_cards = List.length num_overlap in
    let init_num_scratches = Array.create ~len:num_cards 1 in
    get_num_scratches_aux 0 init_num_scratches num_overlap

let get_num_overlap (vals, wins) = 
    let vals_set = Set.of_list (module Int) vals in
    let wins_set = Set.of_list (module Int) wins in
    let overlap = Set.inter vals_set wins_set |> Set.to_list in
    List.length overlap

let get_score (vals, wins) = 
    let vals_set = Set.of_list (module Int) vals in
    let wins_set = Set.of_list (module Int) wins in
    let overlap = Set.inter vals_set wins_set |> Set.to_list in
    let num_overlap = List.length overlap in
    if num_overlap > 0 then Int.of_float (2.**(Float.of_int (num_overlap-1))) else 0

let get_cards_winners card_string = 
    let [_; cards_winners] = String.split card_string ~on:':' in
    let [cards; winners] = String.split cards_winners ~on:'|' in
    let cards2 = String.split cards ~on:' ' |> List.filter ~f:(fun s -> String.compare s "" <> 0) in
    let cards2 = List.map ~f:String.strip cards2 in
    let cards2 = List.map ~f:Int.of_string cards2 in
    let winners2 = String.split winners ~on:' ' |> List.filter ~f:(fun s -> String.compare s "" <> 0) in
    let winners2 = List.map ~f:String.strip winners2 in
    let winners2 = List.map ~f:Int.of_string winners2 in
    (cards2, winners2)

let part1 filename = 
    let string_list = Util.parse_to_lines filename in
    let cards = List.map ~f:get_cards_winners string_list in
    let score = List.map ~f:get_score cards in
    List.reduce_exn ~f:(+) score

let part2 filename = 
    let string_list = Util.parse_to_lines filename in
    let cards = List.map ~f:get_cards_winners string_list in
    let num_overlap = List.map ~f:get_num_overlap cards in
    let num_scratches = get_num_scratches num_overlap in
    List.reduce_exn ~f:(+) ((Array.to_list) num_scratches)
