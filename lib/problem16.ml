open Core

let follow_beams (a1, b1, a0, b0) data =
    let (dimy, dimx) = (Array.length data, Array.length data.(0)) in
    let seen = Array.make_matrix ~dimx:dimy ~dimy:dimx 0 in
    let check_have_seen (i, j) (di, dj) =
        let hs_val = 
            if di > 0 then
                1
            else if di < 0 then
                2
            else if dj > 0 then
                4
            else
                8
        in
        let hs = (seen.(i).(j) land hs_val) <> 0 in
        seen.(i).(j) <- seen.(i).(j) lor hs_val;
        hs
    in
    let out_of_bounds (y, x) =
        if y < 0 || x < 0 || y >= dimy || x >= dimx then
            true
        else
            false
    in
    let rec fb_aux (i, j) (i0, j0) data =
        let (di, dj) = (i-i0, j-j0) in
        if out_of_bounds (i, j) then
            0
        else if check_have_seen (i, j) (di, dj) then
            0
        else
            let curr = data.(i).(j) in
            match curr with
            | '.' ->
                    fb_aux (i+di, j+dj) (i, j) data
            | '-' ->
                    if di <> 0 then
                        let _ = fb_aux (i, j+1) (i, j) data in
                        fb_aux (i, j-1) (i, j) data
                    else
                        fb_aux (i+di, j+dj) (i, j) data
            | '|' ->
                    if dj <> 0 then
                        let _ = fb_aux (i+1, j) (i, j) data in
                        fb_aux (i-1, j) (i, j) data
                    else
                        fb_aux (i+di, j+dj) (i, j) data
            | '/' -> 
                    if di <> 0 then
                        fb_aux (i, j-di) (i, j) data
                    else
                        fb_aux (i-dj, j) (i, j) data
            | '\\' ->
                    if di <> 0 then
                        fb_aux (i, j+di) (i, j) data
                    else
                        fb_aux (i+dj, j) (i, j) data
    in
    let _ = fb_aux (a1, b1) (a0, b0) data in
    seen

let count_energized beams = 
    let fx data = 
        let tmp = Array.filter ~f:(fun x -> x <> 0) data in
        Array.length tmp
    in
    Array.reduce_exn ~f:(+) (Array.map ~f:fx beams)

let display_array data =
    let (dimy, dimx) = (Array.length data, Array.length data.(0)) in
    let rec daux i j data =
        match i with
        | h when h = dimy ->
            Out_channel.output_char stdout '\n';
            0
        | _ ->
            let tmp = data.(i).(j) in
            let x = if tmp <> 0 then '#' else '.' in
            let x1 = if j <> 0 then "" else "\n" in
            Out_channel.output_string stdout x1;
            Out_channel.output_char stdout x;
            let j1 = Int.rem (j+1) dimx in
            let i1 = i + (j+1)/dimx in
            daux i1 j1 data
    in
    daux 0 0 data
    
let start_to_num_energized (dy, dx) array_data (y1, x1) =
    let start = (y1, x1, y1-dy, x1-dx) in
    let beams = follow_beams start array_data in
    count_energized beams

let optimize_beams data = 
    let (dimy, dimx) = (Array.length data, Array.length data.(0)) in
    let range = Util.range 1 dimy in
    let start1 = List.cartesian_product range [0] in
    let start2 = List.cartesian_product range [dimx-1] in
    let start3 = List.cartesian_product [0] range in
    let start4 = List.cartesian_product [dimy-1] range in
    let list1 = List.map ~f:(start_to_num_energized (0, 1) data) start1 in
    let list2 = List.map ~f:(start_to_num_energized (0, -1) data) start2 in
    let list3 = List.map ~f:(start_to_num_energized (1, 0) data) start3 in
    let list4 = List.map ~f:(start_to_num_energized (-1, 0) data) start4 in
    let list_total = list1 @ list2 @ list3 @ list4 in
    List.max_elt ~compare:Int.compare list_total

let part1 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let array_data = List.to_array (List.map ~f:List.to_array list_list_chars) in
    let beams = follow_beams (0, 0, 0, -1) array_data in
    count_energized beams

let part2 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let array_data = List.to_array (List.map ~f:List.to_array list_list_chars) in
    let opt_beam = optimize_beams array_data in
    match opt_beam with
    | None -> 0
    | Some x -> x
    (*let max_beams = optimize_beams array_data in
    count_energized beams*)
    (*display_array beams*)
