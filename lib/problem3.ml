open Core

let rec parse_nums_aux line_num pos nums syms list_chars = 
    match list_chars with
    | [] -> (nums, syms)
    | ('1' .. '9' as x1) :: ('0' .. '9' as x2) :: ('0' .. '9' as x3) :: t ->
            let num1 = (Util.get_num3 x1 x2 x3, (line_num, pos), 3) in
            parse_nums_aux line_num (pos+3) (num1 :: nums) syms t
    | ('1' .. '9' as x1) :: ('0' .. '9' as x2) :: t -> 
            let num1 = (Util.get_num2 x1 x2, (line_num, pos), 2)  in
            parse_nums_aux line_num (pos+2) (num1 :: nums) syms t
    | ('1' .. '9' as x1) :: t -> 
            let num1 = (Util.get_num1 x1, (line_num, pos), 1) in
            parse_nums_aux line_num (pos+1) (num1 :: nums) syms t 
    | '.' :: t -> 
            parse_nums_aux line_num (pos+1) nums syms t
    | h :: t -> 
            let sym1 = (line_num, pos) in
            parse_nums_aux line_num (pos+1) nums (sym1 :: syms) t

let rec parse_nums_aux2 line_num pos nums syms list_chars = 
    match list_chars with
    | [] -> (nums, syms)
    | ('1' .. '9' as x1) :: ('0' .. '9' as x2) :: ('0' .. '9' as x3) :: t ->
            let num1 = (Util.get_num3 x1 x2 x3, (line_num, pos), 3) in
            parse_nums_aux2 line_num (pos+3) (num1 :: nums) syms t
    | ('1' .. '9' as x1) :: ('0' .. '9' as x2) :: t ->
            let num1 = (Util.get_num2 x1 x2, (line_num, pos), 2)  in
            parse_nums_aux2 line_num (pos+2) (num1 :: nums) syms t
    | ('1' .. '9' as x1) :: t -> 
            let num1 = (Util.get_num1 x1, (line_num, pos), 1) in
            parse_nums_aux2 line_num (pos+1) (num1 :: nums) syms t
    | '*' :: t -> 
            let sym1 = (line_num, pos) in
            parse_nums_aux2 line_num (pos+1) nums (sym1 :: syms) t 
    | h :: t -> parse_nums_aux2 line_num (pos+1) nums syms t

let parse_nums_line line_num list_chars = parse_nums_aux line_num 0 [] [] list_chars
let parse_nums_line2 line_num list_chars = parse_nums_aux2 line_num 0 [] [] list_chars
let rec parse_nums_list_aux line_num list_list_chars = 
    match list_list_chars with
    | [] -> ([], [])
    | h :: t -> 
            let (num1, sym1) = parse_nums_line line_num h in
            let (num2, sym2) = parse_nums_list_aux (line_num+1) t in
            (List.append num1 num2, List.append sym1 sym2) 

let rec parse_nums_list_aux2 line_num list_list_chars = 
    match list_list_chars with
    | [] -> ([], [])
    | h :: t -> 
            let (num1, sym1) = parse_nums_line2 line_num h in
            let (num2, sym2) = parse_nums_list_aux2 (line_num+1) t in
            (List.append num1 num2, List.append sym1 sym2) 

let parse_nums_list list_list_chars = parse_nums_list_aux 0 list_list_chars 
let parse_nums_list2 list_list_chars = parse_nums_list_aux2 0 list_list_chars 

let rec is_good_part num syms = 
    let (_, (y0, x0), length) = num in
    match syms with
    | [] -> false
    | (y, x) :: t -> 
            if Int.abs (y0-y) > 1 then
                is_good_part num t
            else if x-x0 < -1 then
                is_good_part num t
            else if x - x0 > length then
                is_good_part num t 
            else
                true

let rec filter_nums nums syms =
    match nums with
    | [] -> []
    | ((value, _, _) as h) :: t -> 
            if is_good_part h syms then
                value :: filter_nums t syms 
            else
                filter_nums t syms

let rec find_neighbors nums curr_neighs sym = 
    let (y, x) = sym in
    match nums with
    | [] -> (sym, curr_neighs)
    | h :: t -> let (value, (y0, x0), length) = h in
                if Int.abs (y0-y) > 1 then
                    find_neighbors t curr_neighs sym
                else if x-x0 < -1 then
                    find_neighbors t curr_neighs sym
                else if x - x0 > length then
                    find_neighbors t curr_neighs sym
                else
                    find_neighbors t (value :: curr_neighs) sym

let rec get_gears sym_and_neighs = 
    match sym_and_neighs with
    | [] -> []
    | (_, vals) :: t -> 
            if List.length vals = 2 then 
                (List.reduce_exn ~f:( * ) vals) :: get_gears t 
            else 
                get_gears t 

let part2 filename =
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let (nums, syms) = parse_nums_list2 list_list_chars in
    let sym_and_neighs = List.map ~f:(find_neighbors nums []) syms in
    let gears = get_gears sym_and_neighs in
    List.reduce_exn ~f:(+) gears

let part1 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let (nums, syms) = parse_nums_list list_list_chars in
    List.reduce_exn ~f:(+) (filter_nums nums syms)


