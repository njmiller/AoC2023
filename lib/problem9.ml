open Core

let is_all_zero list_nums = 
    let is_zero = List.filter ~f:(fun x -> x = 0) list_nums in
    List.length list_nums = List.length is_zero

let rec find_diff list_nums = 
    match list_nums with
    | [] -> []
    | h0 :: (h1 :: t as t2) -> (h1-h0) :: find_diff t2
    | _ -> []

let rec find_next_seq list_nums =
    let diff = find_diff list_nums in
    if is_all_zero diff then
        List.hd_exn list_nums
    else
        let last_val = List.hd_exn (List.rev list_nums) in
        last_val + find_next_seq diff

let find_prev_seq list_nums = find_next_seq (List.rev list_nums)

let part1 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let list_list_nums = List.map ~f:(Util.parse_nums ' ') list_list_chars in
    let next_seqs = List.map ~f:find_next_seq list_list_nums in
    List.reduce_exn ~f:(+) next_seqs

let part2 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let list_list_nums = List.map ~f:(Util.parse_nums ' ') list_list_chars in
    let next_seqs = List.map ~f:find_prev_seq list_list_nums in
    List.reduce_exn ~f:(+) next_seqs
    
