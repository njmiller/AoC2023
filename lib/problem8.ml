open Core

type node = {name : string; left: string; right: string;
             left_num: int; right_num: int}

let chars_to_number chars = 
    let x0 = Char.to_int 'A' in
    let [x3; x2; x1] = List.map ~f:Char.to_int chars in
    (x3-x0)*26*26 + (x2-x0)*26 + (x1-x0)

let name_to_number str = 
    (*let (x2, x1, x0) = List.init (String.length name) ~f:(String.get name) in*)
    let chars = List.init 3 ~f:(String.get str) in
    chars_to_number chars

let check_all_at_Z idxs = 
    let at_Z = List.filter ~f:(fun x -> Int.rem x 26 = 25) idxs in
    List.length idxs = List.length at_Z

let rec update_idxs node_array lr idxs =
    match lr with
    | 'L' -> List.map ~f:(fun x -> node_array.(x).left_num) idxs
    | 'R' -> List.map ~f:(fun x -> node_array.(x).right_num) idxs

let rec traverse_A_to_Z node_array lr_array idxs count =
    if check_all_at_Z idxs then count
    else
        let lr_idx = Int.rem count (Array.length lr_array) in
        let lr_val = lr_array.(lr_idx) in
        let idxs_new = update_idxs node_array lr_val idxs in
        traverse_A_to_Z node_array lr_array idxs_new (count+1)

let rec traverse_to_Z node_array count lr_array idx = 
    let lr_idx = Int.rem count (Array.length lr_array) in
    (*if String.compare node_array.(idx).name "ZZZ" then count*)
    if Int.rem idx 26 = 25 then count
    else match lr_array.(lr_idx) with
         | 'L' -> let idx_new = node_array.(idx).left_num in
                  traverse_to_Z node_array (count+1) lr_array idx_new
         | 'R' -> let idx_new = node_array.(idx).right_num in
                  traverse_to_Z node_array (count+1) lr_array idx_new

let rec traverse_to_ZZZ node_array count lr_array idx = 
    let lr_idx = Int.rem count (Array.length lr_array) in
    (*if String.compare node_array.(idx).name "ZZZ" then count*)
    if idx = 17575 then count
    else match lr_array.(lr_idx) with
         | 'L' -> let idx_new = node_array.(idx).left_num in
                  traverse_to_ZZZ node_array (count+1) lr_array idx_new
         | 'R' -> let idx_new = node_array.(idx).right_num in
                  traverse_to_ZZZ node_array (count+1) lr_array idx_new

let parse_to_node list_chars =
(*let parse_to_node_array list_chars = *)
    let [c0; c1; c2; _; _; _; _; d0; d1; d2; _; _; e0; e1; e2; _] = list_chars in
    let idx0 = chars_to_number [c0; c1; c2] in
    let left_num = chars_to_number [d0; d1; d2] in
    let right_num = chars_to_number [e0; e1; e2] in
    let left_name = String.of_char_list [d0; d1; d2] in
    let right_name = String.of_char_list [e0; e1; e2] in
    let name = String.of_char_list [c0; c1; c2] in
    let node_tmp = {name=name; left=left_name; right=right_name;
                    left_num=left_num; right_num=right_num} in
    (*let _ = array_in.(idx0) <- node_tmp in
    array_in*)
    (idx0, node_tmp)

let rec parse_to_node_array a_idxs array_in list_list_chars = 
    match list_list_chars with
    | [] -> a_idxs
    | h :: t -> let (idx0, node) = parse_to_node h in
                let _ = array_in.(idx0) <- node in
                if Int.rem idx0 26 = 0 then
                    parse_to_node_array (idx0 :: a_idxs) array_in t
                else
                    parse_to_node_array a_idxs array_in t

let part1 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let header :: _ :: list_list_chars = list_list_chars in
    let lr_array = List.to_array header in
    let node_0 = {name = "---"; left="---"; right="---";
                  left_num=(-1); right_num=(-1) } in
    let node_array = Array.create ~len:(26*26*26) node_0 in
    (*let tmp = List.map ~f:parse_to_node_array list_list_chars in
    tmp*)
    let _ = parse_to_node_array [] node_array list_list_chars in
    traverse_to_ZZZ node_array 0 lr_array 0

let part2 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let header :: _ :: list_list_chars = list_list_chars in
    let lr_array = List.to_array header in
    let node_0 = {name = "---"; left="---"; right="---";
                  left_num=(-1); right_num=(-1) } in
    let node_array = Array.create ~len:(26*26*26) node_0 in
    let a_idxs = parse_to_node_array [] node_array list_list_chars in
    (*traverse_A_to_Z node_array lr_array a_idxs 0*)
    let counts = List.map ~f:(traverse_to_Z node_array 0 lr_array) a_idxs in
    Util.calc_lcm counts
