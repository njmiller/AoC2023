open Core

let distance_gone time_accelerate time = 
    time_accelerate*(time - time_accelerate)

let num_ways_to_win time_distance = 
    let (time, distance) = time_distance in
    let time_range = Util.range 1 time in
    let distance2 x = distance_gone x time in
    let distance_gone_list = List.map ~f:distance2 time_range in
    let gone_gt_distance = List.filter ~f:(fun x -> x > distance) distance_gone_list in
    List.length gone_gt_distance
    
let part1 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let [time_chars; distance_chars] = list_list_chars in
    let time_chars = Util.strip_n_chars 11 time_chars in
    let distance_chars = Util.strip_n_chars 11 distance_chars in
    let time_ints = Util.parse_nums ' ' time_chars in
    let distance_ints = Util.parse_nums ' ' distance_chars in
    let time_distance = List.zip_exn time_ints distance_ints in
    let num_ways = List.map ~f:num_ways_to_win time_distance in
    List.reduce_exn ~f:( * ) num_ways 

let rec calc_min_aux i time distance = 
    let xx = (distance_gone i time) - distance in
    if xx > 0 then i else calc_min_aux (i+1) time distance

let calc_min time distance = calc_min_aux 1 time distance

let rec calc_max_aux i time distance = 
    let xx = (distance_gone i time) - distance in
    if xx > 0 then i else calc_max_aux (i-1) time distance

let calc_max time distance = calc_max_aux (time-1) time distance

let part2 filename = 
    (*let (time, distance) = (71530, 940200) in*)
    let (time, distance) = (56977875, 546192711311139) in
    let min_i = calc_min time distance in
    let max_i = calc_max time distance in
    max_i - min_i + 1

let part2_algebra = 
    let (time, distance) = (56977875.0, 546192711311139.0) in
    let (a, b, c) = (-1.0, time, -.distance) in
    let x1 = (-.b +. sqrt(b**2. -. 4.*.a*.c)) /. (2.*.a) in
    let x2 = (-.b -. sqrt(b**2. -. 4.*.a*.c)) /. (2.*.a) in
    let x1i = Int.of_float (Float.round_up x1) in
    let x2i = Int.of_float (Float.round_down x2) in
    x2i - x1i + 1
