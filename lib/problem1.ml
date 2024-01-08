open Core

let extract x = 
    match x with
    | Some t -> t
    | None -> 999

let is_int value = 
    match value with
    | '0' .. '9' -> true
    | _ -> false

let rec find_first_int list_chars = 
    match list_chars with
    | [] -> Some 0
    | h :: t -> if is_int h then (Char.get_digit h) else (find_first_int t)

let rec to_numbers list_chars = 
    match list_chars with
    | [] -> []
    | 'o' :: ('n' :: 'e' :: _ as t2) -> 1 :: to_numbers t2
    | 't' :: ('w' :: 'o' :: _ as t2) -> 2 :: to_numbers t2
    | 't' :: ('h' :: 'r' :: 'e' :: 'e' :: _ as t2) -> 3 :: to_numbers t2
    | 'f' :: ('o' :: 'u' :: 'r' :: _ as t2) -> 4 :: to_numbers t2
    | 'f' :: ('i' :: 'v' :: 'e' :: _ as t2) -> 5 :: to_numbers t2
    | 's' :: ('i' :: 'x' :: _ as t2) -> 6 :: to_numbers t2
    | 's' :: ('e' :: 'v' :: 'e' :: 'n' :: _ as t2) -> 7 :: to_numbers t2
    | 'e' :: ('i' :: 'g' :: 'h' :: 't' :: _ as t2) -> 8 :: to_numbers t2
    | 'n' :: ('i' :: 'n' :: 'e' :: _ as t2) -> 9 :: to_numbers t2
    | '1' :: t -> 1 :: to_numbers t
    | '2' :: t -> 2 :: to_numbers t
    | '3' :: t -> 3 :: to_numbers t
    | '4' :: t -> 4 :: to_numbers t
    | '5' :: t -> 5 :: to_numbers t
    | '6' :: t -> 6 :: to_numbers t
    | '7' :: t -> 7 :: to_numbers t
    | '8' :: t -> 8 :: to_numbers t
    | '9' :: t -> 9 :: to_numbers t
    | _ :: t -> to_numbers t

let calc_calibration_chars2 list_chars = 
    let list_ints = to_numbers list_chars in
    let length = List.length list_ints in
    let first_int = List.nth list_ints 0 in
    let last_int = List.nth list_ints (length-1) in
    10*(extract first_int) + (extract last_int)

let calc_calibration_chars list_chars = 
    let first_int = find_first_int list_chars in
    let rev_list = List.rev list_chars in
    let last_int = find_first_int rev_list in
    10*(extract first_int) + (extract last_int)

let calc_calibration_string str = 
    let list_chars = String.to_list str in
    calc_calibration_chars list_chars

let calc_calibration_string2 str = 
    let list_chars = String.to_list str in
    calc_calibration_chars2 list_chars

let part1 filename = 
    let lines = In_channel.read_lines filename in
    List.map ~f:calc_calibration_string lines |> List.reduce_exn ~f:(+)

let part2 filename = 
    let lines = In_channel.read_lines filename in
    List.map ~f:calc_calibration_string2 lines |> List.reduce_exn ~f:(+)
