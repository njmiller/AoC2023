open Core

let rec list_to_coords start_row list_col_nums =
    match list_col_nums with
    | [] -> []
    | h :: t ->
            let coords = List.map ~f:(fun x -> (x, start_row)) h in
            List.append coords (list_to_coords (start_row+1) t)
    
let compare_coords first x1 x2 = 
    let (y1, z1) = x1 in
    let (y2, z2) = x2 in
    if first then
        Int.compare y1 y2
    else
        Int.compare z1 z2

let rec coords_to_list (row: int) (maxval: int) (coords: (int*int) list) = 
    if row = maxval then
        []
    else
        let coords_good = List.filter ~f:(fun (x,y) -> y = row) coords in
        (*let num_good = List.length coords_good in
        match num_good with
        | 0 -> [] :: coords_to_list (row+1) maxval coords
        | n -> (List.map ~f:(fun (x,y) -> x) coords_good) :: coords_to_list (row+1) maxval coords*)
        (List.sort ~compare:Int.compare (List.map ~f:(fun (x,y) -> x) coords_good)) :: coords_to_list (row+1) maxval coords
        (*List.sort ~compare:(compare coords false) coords*)

let rotate_clockwise (maxval: int) coords = 
    List.map ~f:(fun (x,y) -> (maxval - y - 1, x)) coords

let rotate_cclockwise (maxval: int) coords = 
    List.map ~f:(fun (x,y) -> (y, maxval - x - 1)) coords

let rotate_list maxval list_col_nums = 
    list_to_coords 0 list_col_nums
        |> rotate_cclockwise maxval
        |> coords_to_list 0 maxval

let rotate_rocks maxval rrocks srocks = 
    (rotate_list maxval rrocks, rotate_list maxval srocks)
(*let rotate_coords vals = 
    let nrows = List.length vals in
    let row_nums = 0--nrows in
    let c_tmp_all = List.zip vals row_nums in
    let ctmp_to_coords ctmp = 
        let (vals, row_num) = ctmp in
        List.map ~f:(fun x -> (x, row_num)) vals
    in
    let coords = List.map ~f:ctmp_to_coords c_tmp_all
 *)   

let calc_load maxload nums =
    if List.length nums = 0 then
        0
    else
        List.reduce_exn ~f:(+) (List.map ~f:(fun x -> maxload-x) nums)

let rec move_left (rrocks, bounds) =
    match bounds with
    | [] -> []
    | h1 :: (h2 :: t as t2) ->
            let num = List.length (List.filter ~f:(fun x -> x > h1 && x < h2) rrocks) in
            let nnums = Util.range (h1+1) (h1+1+num) in
            List.append nnums (move_left (rrocks, t2))

    | [h] ->
            let num = List.length (List.filter ~f:(fun x -> x > h) rrocks) in
            Util.range (h+1) (h+1+num)

let rec parse_to_location_line id num list_chars = 
    match list_chars with
    | [] -> []
    | h :: t when Char.compare h id = 0 -> num :: parse_to_location_line id (num+1) t
    | h :: t -> parse_to_location_line id (num+1) t

let shift_left rrocks srocks = 
    let srocks2 = List.map ~f:(fun x -> -1 :: x) srocks in
    let rrocks_bounds = List.zip_exn rrocks srocks2 in
    let rrocks = List.map ~f:move_left rrocks_bounds in
    rrocks

let rec run_n_cycles (maxval:int) (ncycles:int) (rrocks: int list list) (srocks: int list list) =
    (*let str = if Int.rem ncycles 1000 = 0 then
        Int.to_string (List.reduce_exn ~f:(+) (List.map ~f:(calc_load maxval) rrocks))
    else
        "" in*)
    let str = Int.to_string (List.reduce_exn ~f:(+) (List.map ~f:(calc_load maxval) rrocks)) in
    Out_channel.output_string stdout (Int.to_string ncycles);
    Out_channel.output_string stdout " ";
    Out_channel.output_string stdout str;
    Out_channel.output_string stdout "\n";
        
    match ncycles with
    | 0 -> rrocks
    | n ->
            (*North, West, South, East*)
            let rrocks = shift_left rrocks srocks in
            let (rrocks, srocks) = rotate_rocks maxval rrocks srocks in
            let rrocks = shift_left rrocks srocks in
            let (rrocks, srocks) = rotate_rocks maxval rrocks srocks in
            let rrocks = shift_left rrocks srocks in
            let (rrocks, srocks) = rotate_rocks maxval rrocks srocks in
            let rrocks = shift_left rrocks srocks in
            let (rrocks, srocks) = rotate_rocks maxval rrocks srocks in

            run_n_cycles maxval (ncycles-1) rrocks srocks

(*
let find_cycle data = 
    let rec fc_aux n data1 data2 = 
        let h1 :: t1 = data1 in
        let h2 :: _ :: t2 = data2 in
        if h1 = h2 then
            n
        else
            fc_aux (n+1) h1 h2
    in
    let data2 = List.tl data1 in
    fc_aux n data1 data2
*)

let part1 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let maxload = List.length list_list_chars in
    let list_list_chars = List.transpose_exn list_list_chars in
    let loc1 = List.map ~f:(parse_to_location_line 'O' 0) list_list_chars in
    let loc2 = List.map ~f:(parse_to_location_line '#' 0) list_list_chars in
    let loc2 = List.map ~f:(fun x -> -1 :: x) loc2 in
    let rrocks_bounds = List.zip_exn loc1 loc2 in
    let rrocks = List.map ~f:move_left rrocks_bounds in
    List.reduce_exn ~f:(+) (List.map ~f:(calc_load maxload) rrocks)
    (*List.map ~f:calc_load lnums*)

let part2 filename =
    let ncycles = 1000 in
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let maxload = List.length list_list_chars in
    let list_list_chars = List.transpose_exn list_list_chars in
    let rrocks = List.map ~f:(parse_to_location_line 'O' 0) list_list_chars in
    let srocks = List.map ~f:(parse_to_location_line '#' 0) list_list_chars in
    let rrocks = run_n_cycles maxload ncycles rrocks srocks in
    List.reduce_exn ~f:(+) (List.map ~f:(calc_load maxload) rrocks)

(*
let problem14_part2b filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let maxload = List.length list_list_chars in
    let list_list_chars = List.transpose_exn list_list_chars in
    let rrocks = List.map ~f:(parse_to_location_line 'O' 0) list_list_chars in
    let srocks = List.map ~f:(parse_to_location_line '#' 0) list_list_chars in
    let load_at_cycle = get_load_at_cycles ncycles rrocks srocks in
    let length_cycle = find_cycle load_at_cycle in
    length_cycle
*)

