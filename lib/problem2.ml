open Core

let rec calc_power [r; g; b] chars = 
    match chars with
    | [] -> r*g*b
    | ('1' .. '9' as h) :: ' ' :: 'b' :: 'l' :: 'u' :: 'e' :: t ->
            let x = Char.get_digit_exn h in 
            if x > b then calc_power [r; g; x] t else calc_power [r; g; b] t 
    | ('1' .. '9' as h) :: ('0' .. '9' as h2) :: ' ' :: 'b' :: 'l' :: 'u' :: 'e' :: t ->
            let x = Util.get_num2 h h2 in
            if x > b then calc_power [r; g; x] t else calc_power [r; g; b] t 
    | ('1' .. '9' as h) :: ' ' :: 'r' :: 'e' :: 'd' :: t ->
            let x = Char.get_digit_exn h in
            if x > r then calc_power [x; g; b] t else calc_power [r; g; b] t 
    | ('1' .. '9' as h) :: ('0' .. '9' as h2) :: ' ' :: 'r' :: 'e' :: 'd' :: t -> 
            let x = Util.get_num2 h h2 in
            if x > r then calc_power [x; g; b] t else calc_power [r; g; b] t 
    | ('1' .. '9' as h) :: ' ' :: 'g' :: 'r' :: 'e' :: 'e' :: 'n' :: t -> 
            let x = Char.get_digit_exn h in
            if x > g then calc_power [r; x; b] t else calc_power [r; g; b] t 
    | ('1' .. '9' as h) :: ('0' .. '9' as h2) :: ' ' :: 'g' :: 'r' :: 'e' :: 'e' :: 'n' :: t ->
            let x = Util.get_num2 h h2 in
            if x > g then calc_power [r; x; b] t else calc_power [r; g; b] t 
    (*| ';' :: t -> calc_power [r; g; b] t
    | ',' :: t -> calc_power [r; g; b] t
    | ' ' :: t -> calc_power [r; g; b] t*)
    | _ :: t -> calc_power [r; g; b] t

let rec check_valid_game chars = 
    let valid_red = 12 in
    let valid_green = 13 in
    let valid_blue = 14 in
    match chars with
    | [] -> true
    | ('1' .. '9' as h) :: ' ' :: 'b' :: 'l' :: 'u' :: 'e' :: t -> 
            if Char.get_digit_exn h > valid_blue then false else check_valid_game t 
    | ('1' .. '9' as h) :: ('0' .. '9' as h2) :: ' ' :: 'b' :: 'l' :: 'u' :: 'e' :: t -> 
            if (Util.get_num2 h h2) > valid_blue then false else check_valid_game t 
    | ('1' .. '9' as h) :: ' ' :: 'r' :: 'e' :: 'd' :: t -> 
            if Char.get_digit_exn h > valid_red then false else check_valid_game t 
    | ('1' .. '9' as h) :: ('0' .. '9' as h2) :: ' ' :: 'r' :: 'e' :: 'd' :: t -> 
            if (Util.get_num2 h h2) > valid_red then false else check_valid_game t 
    | ('1' .. '9' as h) :: ' ' :: 'g' :: 'r' :: 'e' :: 'e' :: 'n' :: t -> 
            if Char.get_digit_exn h > valid_green then false else check_valid_game t 
    | ('1' .. '9' as h) :: ('0' .. '9' as h2) :: ' ' :: 'g' :: 'r' :: 'e' :: 'e' :: 'n' :: t -> 
            if (Util.get_num2 h h2) > valid_green then false else check_valid_game t 
    (*| ';' :: t -> check_valid_game t
    | ',' :: t -> check_valid_game t
    | ' ' :: t -> check_valid_game t*)
    | _ :: t -> check_valid_game t

let calc_valid_game str = 
    let list_chars = String.to_list str in
    match list_chars with
    | [] -> 0
    | 'G' :: 'a' :: 'm' :: 'e' :: ' ' :: h :: ':' :: ' ' :: t -> if check_valid_game t then Char.get_digit_exn h else 0
    | 'G' :: 'a' :: 'm' :: 'e' :: ' ' :: h :: h2 :: ':' :: ' ' :: t -> if check_valid_game t then (Util.get_num2 h h2) else 0
    | 'G' :: 'a' :: 'm' :: 'e' :: ' ' :: _ :: _ :: _ :: ':' :: ' ' :: t -> if check_valid_game t then 100 else 0
    | _ :: _ -> 0

let calc_power2 str = 
    let list_chars = String.to_list str in
    match list_chars with
    | [] -> 0
    | 'G' :: 'a' :: 'm' :: 'e' :: ' ' :: _ :: ':' :: ' ' :: t -> calc_power [0; 0; 0] t
    | 'G' :: 'a' :: 'm' :: 'e' :: ' ' :: _ :: _ :: ':' :: ' ' :: t -> calc_power [0; 0; 0] t
    | 'G' :: 'a' :: 'm' :: 'e' :: ' ' :: _ :: _ :: _ :: ':' :: ' ' :: t -> calc_power [0; 0; 0] t
    | _ :: _ -> 0

let rec calc_valid_games data = 
    match data with
    | [] -> 0
    | h :: t -> (calc_valid_game h) + (calc_valid_games t)

let rec calc_sum_powers (data: string list) = 
    match data with
    | [] -> 0
    | h :: t -> (calc_power2 h) + (calc_sum_powers t)

let part1 filename =
    let file = In_channel.create filename in
    let lines = In_channel.input_lines file in
    calc_valid_games lines

let part2 filename =
    let file = In_channel.create filename in
    let lines = In_channel.input_lines file in
    calc_sum_powers lines
