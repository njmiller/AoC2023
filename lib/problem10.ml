open Core

let rec calc_in_aux num inout prev list_chars = 
    match list_chars with
    | [] -> num
    | '.' :: t -> calc_in_aux (num+inout) inout '.' t
    | '-' :: t -> calc_in_aux num inout prev t
    | '|' :: t -> calc_in_aux num (1-inout) '|' t
    | 'F' :: t -> calc_in_aux num inout 'F' t
    | 'L' :: t -> calc_in_aux num inout 'L' t
    | '7' :: t -> if Char.compare prev 'F' = 0 then 
                      calc_in_aux num inout '-' t
                  else
                      calc_in_aux num (1-inout) '-' t
    | 'J' :: t -> if Char.compare prev 'L' = 0 then
                      calc_in_aux num inout '-' t
                  else
                      calc_in_aux num (1-inout) '-' t

let calc_in list_chars = calc_in_aux 0 0 '-' list_chars

let calc_in_out_hor num inout prev arr_val = 
    match arr_val with
    | '.' -> ((num+inout), inout, '.')
    | '-' -> (num, inout, prev)
    | '|' -> (num, (1-inout), '|')
    | 'F' -> (num, inout, 'F')
    | 'L' -> (num, inout, 'L')
    | '7' -> if Char.compare prev 'F' = 0 then (num, inout, '-')
             else
                 (num, (1-inout), '-')
    | 'J' -> if Char.compare prev 'L' = 0 then (num, inout, '-')
             else
                 (num, (1-inout), '-')

let print_idx idxy idxx = 
    let _ = Out_channel.output_string stdout (Int.to_string idxy) in
    let _ = Out_channel.output_char stdout ' ' in
    let _ = Out_channel.output_string stdout (Int.to_string idxx) in
    let _ = Out_channel.output_char stdout '\n' in
    0

let rec calc_in_out_aux2 num inout prev idx arr0 = 
    let (dimy, dimx) = (Array.length arr0, Array.length arr0.(0)) in
    let (idxy, idxx) = idx in 
    if idxy = dimy then (num, arr0)
    (*else if idxx = 0 then
        let arr_val = arr0.(idxy).(idxx) in
        let _ = arr0.(idxy).(idxx) <- '.' in
        calc_in_out_aux2 num 0 arr_val (idxy, idxx+1) arr0*)
    else
        let arr_val = arr0.(idxy).(idxx) in
        let (num1, inout1, prev1) = calc_in_out_hor num inout prev arr_val in
        let _ = if num1 > num then 
                    arr0.(idxy).(idxx) <- '1'
                else if (Char.compare arr_val '.' = 0) then 
                    arr0.(idxy).(idxx) <- '0' 
                else arr0.(idxy).(idxx) <- '.' in
        if idxx = dimx-1 then
            calc_in_out_aux2 num1 0 '.' (idxy+1, 0) arr0
        else
            calc_in_out_aux2 num1 inout1 prev1 (idxy, idxx+1) arr0

let rec calc_in_out_aux num inout prev idx arr0 =
    let (dimy, dimx) = (Array.length arr0, Array.length arr0.(0)) in
    let (idxy, idxx) = idx in 
    let (prevy, prevx) = prev in
    let (inoutx0, inoutx) = inout in
    (*let _ = print_idx idxy idxx in*)
    if idxy = dimy then (num, arr0) 
    else if idxx = 0 then
        let arr_val = arr0.(idxy).(idxx) in
        let _ = arr0.(idxy).(idxx) <- '.' in
        let (num1, inoutx0, prevy) = (num, 0, '.') in
        calc_in_out_aux num1 (inoutx0, inoutx0) (prevy, arr_val) (idxy, idxx+1) arr0
    else
        let arr_val = arr0.(idxy).(idxx) in
        let (num1, inoutx, prevx) = calc_in_out_hor num inoutx prevx arr_val in
        let _ = if num1 > num then 
                    arr0.(idxy).(idxx) <- '1'
                else if (Char.compare arr_val '.' = 0) then 
                    arr0.(idxy).(idxx) <- '0' 
                else arr0.(idxy).(idxx) <- '.' in
        if idxx = dimx-1 then
            calc_in_out_aux num1 (0, 0) (prevy, prevx) (idxy+1, 0) arr0
        else
            calc_in_out_aux num1 (inoutx0, inoutx) (prevy, prevx) (idxy, idxx+1) arr0

let calc_in_out arr0 = calc_in_out_aux 0 (0,0) ('.', '.') (0, 0) arr0

let calc_in_out2 arr0 = calc_in_out_aux2 0 0 '.' (0, 0) arr0

let update_idx cidx pidx arr0 =
    let (cidxy, cidxx) = cidx in
    let (pidxy, pidxx) = pidx in
    match arr0.(cidxy).(cidxx) with
    | '|' -> let (idx0y, idx0x) = (cidxy-1, cidxx) in
             let (idx1y, idx1x) = (cidxy+1, cidxx) in
             if idx0y = pidxy && idx0x = pidxx then
                (idx1y, idx1x) else (idx0y, idx0x)
    | '-' -> let (idx0y, idx0x) = (cidxy, cidxx-1) in
             let (idx1y, idx1x) = (cidxy, cidxx+1) in
             if idx0y = pidxy && idx0x = pidxx then
                (idx1y, idx1x) else (idx0y, idx0x)
    | 'L' -> let (idx0y, idx0x) = (cidxy-1, cidxx) in
             let (idx1y, idx1x) = (cidxy, cidxx+1) in
             if idx0y = pidxy && idx0x = pidxx then
                (idx1y, idx1x) else (idx0y, idx0x)
    | 'J' -> let (idx0y, idx0x) = (cidxy-1, cidxx) in
             let (idx1y, idx1x) = (cidxy, cidxx-1) in
             if idx0y = pidxy && idx0x = pidxx then
                (idx1y, idx1x) else (idx0y, idx0x)
    | '7' -> let (idx0y, idx0x) = (cidxy+1, cidxx) in
             let (idx1y, idx1x) = (cidxy, cidxx-1) in
             if idx0y = pidxy && idx0x = pidxx then
                (idx1y, idx1x) else (idx0y, idx0x)
    | 'F' -> let (idx0y, idx0x) = (cidxy+1, cidxx) in
             let (idx1y, idx1x) = (cidxy, cidxx+1) in
             if idx0y = pidxy && idx0x = pidxx then
                (idx1y, idx1x) else (idx0y, idx0x)

let rec find_length_aux sidx cidx pidx count arr0 arr1 = 
    let (sidxy, sidxx) = sidx in
    let (cidxy, cidxx) = cidx in
    let _ = arr1.(cidxy).(cidxx) <- arr0.(cidxy).(cidxx) in
    if sidxy = cidxy && sidxx = cidxx then
        (count, arr1)
    else
        let nidx = update_idx cidx pidx arr0 in
        find_length_aux sidx nidx cidx (count+1) arr0 arr1

let find_length_loop sidx arr0 = 
    let (idxy, idxx) = sidx in
    let cidx = (idxy+1, idxx) in
    let (dimy, dimx) = (Array.length arr0, Array.length arr0.(0)) in
    let arr1 = Array.make_matrix ~dimy ~dimx '.' in
    find_length_aux sidx cidx sidx 1 arr0 arr1
    
let rec find_s_aux2 idxx list_chars = 
    match list_chars with
    | [] -> -1
    | 'S' :: t -> idxx
    | h :: t -> find_s_aux2 (idxx+1) t

let rec find_s_aux idxy list_list_chars = 
    match list_list_chars with
    | [] -> (-1, -1)
    | h :: t -> let tmp = find_s_aux2 0 h in
                if tmp >= 0 then (idxy, tmp) else find_s_aux (idxy+1) t

let find_s list_list_chars = find_s_aux 0 list_list_chars

let ll_to_aa sll = Array.of_list (List.map ~f:Array.of_list sll)

let rec print_arr_aux idxy idxx arr0 = 
    let (dimy, dimx) = (Array.length arr0, Array.length arr0.(0)) in
    let _ = Out_channel.output_char stdout arr0.(idxy).(idxx) in
    if idxx = (dimx-1) then
        let _ = Out_channel.output_char stdout '\n' in
        if idxy = (dimy-1) then 
            0
        else
            print_arr_aux (idxy+1) 0 arr0
    else
        print_arr_aux idxy (idxx+1) arr0

let print_arr arr0 = print_arr_aux 0 0 arr0

let rec print_list tmp = 
    match tmp with
    | [] -> Out_channel.output_char stdout '\n'
    | h :: t -> let _ = Out_channel.output_char stdout h in
                print_list t

let part1 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let array_data = ll_to_aa list_list_chars in
    let s_idx = find_s list_list_chars in
    let (length_loop, array_loop) = find_length_loop s_idx array_data in
    length_loop / 2

let part2 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let array_data = ll_to_aa list_list_chars in
    let (tmpy, tmpx) as s_idx = find_s list_list_chars in
    let _ = array_data.(tmpy).(tmpx) <- '|' in
    let (length_loop, array_loop) = find_length_loop s_idx array_data in
    let (in_loop, arr2) = calc_in_out2 array_loop in
    in_loop

let part2_ver2 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let array_data = ll_to_aa list_list_chars in
    let (tmpy, tmpx) as s_idx = find_s list_list_chars in
    let _ = array_data.(tmpy).(tmpx) <- '|' in
    let (length_loop, array_loop) = find_length_loop s_idx array_data in
    let list_loop = List.map ~f:Array.to_list (Array.to_list array_loop) in
    List.reduce_exn ~f:(+) (List.map ~f:calc_in list_loop)

