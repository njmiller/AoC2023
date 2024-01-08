open Core

let rec calc_expansion x0 x1 expansion exp_const =
    let (x0, x1) = if x0 > x1 then (x1, x0) else (x0, x1) in
    match expansion with
    | [] -> 0
    | h :: t -> if h > x1 then
                    0
                else if h > x0 && h < x1 then
                    (exp_const - 1) + calc_expansion x0 x1 t exp_const
                else
                    calc_expansion x0 x1 t exp_const


let calc_distance star1 star2 expansion exp_const = 
    let (xexp, yexp) = expansion in
    let (x1, y1) = star1 in
    let (x2, y2) = star2 in
    Int.abs (x2-x1) + Int.abs (y2-y1) + (calc_expansion x1 x2 xexp exp_const) + (calc_expansion y1 y2 yexp exp_const) 

let rec ctd_aux star1 star_list expansion exp_const = 
    match star_list with
    | [] -> 0
    | h :: t -> (calc_distance star1 h expansion exp_const) + (ctd_aux star1 t expansion exp_const)

let rec calc_total_distance star_list expansion exp_const = 
    match star_list with
    | [a] -> 0
    | h :: t -> (ctd_aux h t expansion exp_const) + calc_total_distance t expansion exp_const

let rec find_missing_nums currval data = 
    match data with
    | [] -> []
    | d0 :: d1 -> if currval < d0 then
                      currval :: find_missing_nums (currval+1) data
                  else
                      find_missing_nums (currval+1) d1

let rec unique_list data =
    match data with
    | [a] -> [a]
    | [] -> []
    | h0 :: (h1 :: t as t2) -> if (h0 = h1) then unique_list t2
                               else h0 :: unique_list t2

let find_empty_cols_rows star_list =
    let (xlocs, ylocs) = List.unzip star_list in
    let xmiss = List.sort ~compare:Int.compare xlocs 
                |> unique_list
                |> (find_missing_nums 0) in
    let ymiss = List.sort ~compare:Int.compare ylocs 
                |> unique_list
                |> (find_missing_nums 0) in
    (xmiss, ymiss)
    
let part1 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let star_list = Util.parse_to_location '#' list_list_chars in
    let expansion = find_empty_cols_rows star_list in
    let exp_const = 2 in
    calc_total_distance star_list expansion exp_const

let part2 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let star_list = Util.parse_to_location '#' list_list_chars in
    let expansion = find_empty_cols_rows star_list in
    let exp_const = 1000000 in
    calc_total_distance star_list expansion exp_const
    
