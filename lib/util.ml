open Core

let rec combnk k lst =
    if k = 0 then
        [[]]
    else
        let rec inner = function
            | []      -> []
            | x :: xs -> List.map ~f:(fun z -> x :: z) (combnk (k - 1) xs) :: inner xs in
            List.concat (inner lst)

let divide_if_factor y x = 
    if Int.rem x y = 0 then (x/y, 1) else (x, 0)

let rec lcm_aux factors curr_val nums = 
    let (nums_new, has_divide) = List.unzip (List.map ~f:(divide_if_factor curr_val) nums) in
    if List.reduce_exn ~f:(+) nums_new = List.length nums_new then
        curr_val :: factors
    else if List.reduce_exn ~f:(+) has_divide = 0 then
        lcm_aux factors (curr_val+1) nums
    else
        lcm_aux (curr_val :: factors) 2 nums_new

let calc_lcm int_array = 
    let factor_array = lcm_aux [] 2 int_array in
    List.reduce_exn ~f:( * ) factor_array

let extract x = 
    match x with
    | Some t -> t
    | None -> 999

let get_num1 x1 = 
    Char.get_digit_exn x1

let get_num2 x1 x2 = 
    let y1 = Char.get_digit_exn x1 in
    let y2 = Char.get_digit_exn x2 in
    10*y1 + y2

let get_num3 x1 x2 x3 = 
    let y1 = Char.get_digit_exn x1 in
    let y2 = Char.get_digit_exn x2 in
    let y3 = Char.get_digit_exn x3 in
    100*y1 + 10*y2 + y3

let rec lines_to_chars lines = 
    match lines with
    | [] -> []
    | h :: t -> (List.init (String.length h) ~f:(String.get h)) :: lines_to_chars t

let parse_to_lines filename = 
    let file = In_channel.create filename in
    In_channel.input_lines file

let parse_to_list_list_chars filename = 
    let file = In_channel.create filename in
    let lines = In_channel.input_lines file in
    lines_to_chars lines

let rec strip_lines list_list_chars n = 
    match n with
    | 0 -> list_list_chars
    | _ -> let t = List.tl_exn list_list_chars in
           strip_lines t (n-1)

let rec strip_chars list_chars = 
    match list_chars with
    | ' ' :: t -> strip_chars t
    | _ -> list_chars

let rec strip_n_chars n list_chars = 
    match list_chars with
    | [] -> []
    | _ :: t -> if n = 0 then list_chars else strip_n_chars (n-1) t

let range from until =
  List.init (until - from) ~f:(fun i -> i + from)

let (--) = range

let rec parse_nums_aux (pn: int) (nums : int list) (num_tmp : int) sep list_chars =
    match list_chars with
    | [] -> (pn*num_tmp) :: nums
    | ('0' .. '9' as h) :: t -> let x1 = get_num1 h in
                                if num_tmp >= 0 then parse_nums_aux pn nums (10*num_tmp + x1) sep t
                                else parse_nums_aux pn nums x1 sep t
    (*| ' ' :: t -> parse_nums_aux (num_tmp :: nums) 0 sep t*)
    | h :: t when (Char.compare h sep = 0) -> if num_tmp >= 0 then parse_nums_aux 1 (pn*num_tmp :: nums) (-1) sep t
                                              else parse_nums_aux pn nums (-1) sep t
    | '-' :: t -> parse_nums_aux (-1) nums num_tmp sep t
    | _ :: t -> parse_nums_aux 1 nums num_tmp sep t
    
let parse_nums sep list_chars = List.rev (parse_nums_aux 1 [] (-1) sep list_chars)

let parse_num list_chars = 
    let num_list = parse_nums ' ' list_chars in
    let num = List.hd_exn num_list in
    num

let rec parse_line_aux x id list_chars =
    match list_chars with
    | [] -> []
    | h :: t when (Char.compare h id = 0) -> x :: parse_line_aux (x+1) id t
    | _ :: t -> parse_line_aux (x+1) id t

let parse_line line_num id list_chars = 
    let xvals = parse_line_aux 0 id list_chars in
    List.map ~f:(fun x -> (x, line_num)) xvals

let rec parse_to_location_aux line_num id list_list_chars = 
    match list_list_chars with
    | [] -> []
    | h :: t -> List.append (parse_line line_num id h) (parse_to_location_aux (line_num+1) id t)

let parse_to_location id list_list_chars = parse_to_location_aux 0 id list_list_chars



