open Core

let rec calc_diff_aux num list1 list2 = 
    match list1 with
    | [] -> num
    | h1 :: t1 -> 
            let h2 :: t2 = list2 in
            if Char.compare h1 h2 <> 0 then
                calc_diff_aux (num+1) t1 t2
            else
                calc_diff_aux num t1 t2

let calc_diff list1 list2 = calc_diff_aux 0 list1 list2

let rec verify_refl i j grid =
    let len = Array.length grid in
    if i + 1 >= len then
        false
    else if j > i || i + j + 1 >= len then 
        true
    else if List.compare Char.compare grid.(i-j) grid.(i+j+1) <> 0 then
        false
    else
        verify_refl i (j+1) grid

let rec verify_smudge_refl i j k grid = 
    let len = Array.length grid in
    if k > 1 then
        false
    else if i + 1 >= len then
        false
    else if (j > i || i + j + 1 >= len) && k = 1 then 
        true
    else if (j > i || i + j + 1 >= len) then
        false
    else
        let diff = calc_diff grid.(i-j) grid.(i+j+1) in
        verify_smudge_refl i (j+1) (k+diff) grid
        
let rec calc_refl_aux i grid =
    let len = Array.length grid in
    if i = len then
        0
    else if verify_refl i 0 grid then
        i + 1
    else 
        calc_refl_aux (i+1) grid

let rec calc_smudge_aux i grid = 
    let len = Array.length grid in
    if i = len then
        0
    else if verify_smudge_refl i 0 0 grid then
        i + 1
    else
        calc_smudge_aux (i+1) grid

let calc_refl grid = 
    let grid_tmp = List.to_array grid in
    calc_refl_aux 0 grid_tmp

let calc_hor_refl grid = calc_refl (List.transpose_exn grid)

let calc_smudge_refl grid = 
    let grid_tmp = List.to_array grid in
    calc_smudge_aux 0 grid_tmp

let calc_smudge_hor_refl grid = calc_smudge_refl (List.transpose_exn grid)

let rec parse_to_grids_aux allg currg list_list_chars = 
    match list_list_chars with
    | [] -> List.rev ((List.rev currg) :: allg)
    | [] :: t -> parse_to_grids_aux ((List.rev currg) :: allg) [] t
    | h :: t -> parse_to_grids_aux allg (h :: currg) t

let parse_to_grids list_list_chars = 
    parse_to_grids_aux [] [] list_list_chars

let part1 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let list_grids = parse_to_grids list_list_chars in
    let vert_refl = List.map ~f:calc_refl list_grids in
    let hor_refl = List.map ~f:calc_hor_refl list_grids in
    List.reduce_exn ~f:(+) hor_refl + 100*(List.reduce_exn ~f:(+) vert_refl)

let part2 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let list_grids = parse_to_grids list_list_chars in
    let vert_refl = List.map ~f:calc_smudge_refl list_grids in
    let hor_refl = List.map ~f:calc_smudge_hor_refl list_grids in
    List.reduce_exn ~f:(+) hor_refl + 100*(List.reduce_exn ~f:(+) vert_refl)
    
