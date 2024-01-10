open Core

type digplan = {dir: char; num: int; color: string}

let process_line list_chars = 
    match list_chars with
    | d :: ' ' :: n :: ' ' :: '(' :: '#' :: t ->
            let n2 = Char.get_digit_exn n in
            let _ :: color_rev = List.rev t in
            let color = List.rev color_rev in
            {dir=d;num=n2;color=(String.of_char_list color)}
    | d :: ' ' :: n0 :: n1 :: ' ' :: '(' :: '#' :: t ->
            let n2 = (Char.get_digit_exn n0)*10 + (Char.get_digit_exn n1) in
            let _ :: color_rev = List.rev t in
            let color = List.rev color_rev in
            {dir=d;num=n2;color=(String.of_char_list color)}

let hex_to_dec hexstring = int_of_string ("0x" ^ hexstring)

let rec to_digplan list_list_chars = 
    match list_list_chars with
    | [] -> []
    | h :: t -> (process_line h) :: (to_digplan t)

let color_to_d_num [a0; a1; a2; a3; a4; a5; a6] = 
    let hex_num = String.of_char_list [a0; a1; a2; a3; a4] in
    let d = (match a5 with
             | '0' -> 'R'
             | '1' -> 'D'
             | '2' -> 'L'
             | '3' -> 'U'
             ) in
    let dec_num = hex_to_dec hex_num in
    {dir=d; num=dec_num; color="AAA"}

let process_line2 list_chars = 
    match list_chars with
    | d :: ' ' :: n :: ' ' :: '(' :: '#' :: t ->
            color_to_d_num t
    | d :: ' ' :: n0 :: n1 :: ' ' :: '(' :: '#' :: t ->
            color_to_d_num t

let rec to_digplan2 list_list_chars = 
    match list_list_chars with
    | [] -> []
    | h :: t -> (process_line2 h) :: (to_digplan2 t)

let rec s_aux path sum (x,y) diglist = 
    match diglist with
    | [] -> (path, sum)
    | h :: t ->
            let (x_new, y_new) = (match h.dir with
                        | 'R' -> (x + h.num, y)
                        | 'L' -> (x - h.num, y)
                        | 'U' -> (x, y - h.num)
                        | 'D' -> (x, y + h.num)
                        ) in
            let sum1 = (x*y_new) - (y*x_new) in
            let sum = sum + sum1 in
            let path = path + h.num in
            s_aux path sum (x_new, y_new) t 

let rec shoelace_calc diglist = 
    let (path, sum) = (s_aux 0 0 (0, 0) diglist) in

    (* This is Pick's Theorem. The area enclosed by the polygon is A = I + P/2 - 1 where I = grid points interior and P is grid points on boundary. In out calculation, |sum| is 2*A and the path is P. The result we want is I+P which is (A-P/2) + 1 + P = A + P/2 + 1 *)
    path/2 + (abs sum)/2 + 1

let part1 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let digdirs = to_digplan list_list_chars in
    shoelace_calc digdirs

let part2 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let digdirs = to_digplan2 list_list_chars in
    shoelace_calc digdirs

