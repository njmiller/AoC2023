open Core

let rec expand_seeds_aux seeds_new seeds = 
    match seeds with
    | [] -> seeds_new
    | h1 :: h2 :: t ->
            let seed_start = h2 in
            let seed_end = h2 + h1 - 1 in
            let seeds_range = (seed_start, seed_end) in
            expand_seeds_aux (seeds_range :: seeds_new) t

let get_seed_ranges seeds = expand_seeds_aux [] seeds 

let rec apply_map map num = 
    match map with
    | [] -> num
    | h :: t -> let [range; source; dest] = h in
                let delta = num - source in
                let in_range = (delta >= 0 && delta < range) in
                if in_range then dest + delta else apply_map t num

let rec apply_map_range map range =
    let (in_min, in_max) = range in
    match map with
    | [] -> [range]
    | h :: t -> let [length; source; dest] = h in
                let source_max = source+length-1 in
                let maxltmin = in_max < source in
                let mingtmax = in_min > source_max in
                let maxgtmax = in_max > source_max in
                let minltmin = in_min < source in
                let distinct = mingtmax || maxltmin in
                let threeranges = minltmin && maxgtmax in
                let onerange = (in_min >= source) && (in_max <= source_max) in
                if distinct then apply_map_range t range 
                else if threeranges then
                    let range1 = (in_min, source-1) in
                    let range2 = (source_max+1, in_max) in
                    let dest3 = (dest, dest+length-1) in
                    let dest1 = apply_map_range t range1 in
                    let dest2 = apply_map_range t range2 in
                    List.append (List.append dest1 dest2) [dest3]
                else if onerange then
                    let delta0 = in_min - source in
                    let length = in_max - in_min in
                    [(dest+delta0, dest+delta0+length)]
                else if minltmin then
                    let range1 = (in_min, source-1) in
                    let delta = in_max - source in
                    let dest2 = (dest, dest+delta) in
                    List.append (apply_map_range t range1) [dest2]
                else
                    let range1 = (source_max+1, in_max) in
                    let delta = in_min - source in
                    let dest2 = (dest+delta, dest+length-1) in
                    List.append (apply_map_range t range1) [dest2]

let rec apply_map_ranges map ranges = 
    match ranges with
    | [] -> []
    | h :: t -> List.append (apply_map_range map h) (apply_map_ranges map t)

let parse_seeds list_chars = 
    let 's' :: 'e' :: 'e' :: 'd' :: 's' :: ':' :: ' ' :: list2 = list_chars in
    Util.parse_nums_aux 1 [] 0 ' ' list2

let rec mapping_aux list_list_chars map =
    match list_list_chars with
    | [] -> map
    | [] :: t -> map
    | h :: t -> (Util.parse_nums_aux 1 [] 0 ' ' h) :: (mapping_aux t map)

let parse_mapping list_list_chars = 
    let map = mapping_aux list_list_chars [] in
    let length_list = List.length list_list_chars in
    let length_strip_tmp = List.length map + 2 in
    let length_strip = if length_strip_tmp > length_list then length_list else length_strip_tmp in
    let list_out = Util.strip_lines list_list_chars length_strip in
    (map, list_out)

let part1 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let seed_line :: list_list_chars = list_list_chars in 
    let seeds = parse_seeds seed_line in
    let _ :: stsm_line :: list_list_chars = list_list_chars in
    let (stsm, list_list_chars) = parse_mapping list_list_chars in
    let (stfm, list_list_chars) = parse_mapping list_list_chars in
    let (ftwm, list_list_chars) = parse_mapping list_list_chars in
    let (wtlm, list_list_chars) = parse_mapping list_list_chars in
    let (lttm, list_list_chars) = parse_mapping list_list_chars in
    let (tthm, list_list_chars) = parse_mapping list_list_chars in
    let (htlm, list_list_chars) = parse_mapping list_list_chars in
    let loc = List.map ~f:(apply_map stsm) seeds
            |> List.map ~f:(apply_map stfm)
            |> List.map ~f:(apply_map ftwm)
            |> List.map ~f:(apply_map wtlm)
            |> List.map ~f:(apply_map lttm)
            |> List.map ~f:(apply_map tthm)
            |> List.map ~f:(apply_map htlm) in
    Set.min_elt_exn (Set.of_list (module Int) loc)

let part2 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let seed_line :: list_list_chars = list_list_chars in 
    let seeds = parse_seeds seed_line in
    let seed_range = get_seed_ranges seeds in
    let _ :: stsm_line :: list_list_chars = list_list_chars in
    let (stsm, list_list_chars) = parse_mapping list_list_chars in
    let (stfm, list_list_chars) = parse_mapping list_list_chars in
    let (ftwm, list_list_chars) = parse_mapping list_list_chars in
    let (wtlm, list_list_chars) = parse_mapping list_list_chars in
    let (lttm, list_list_chars) = parse_mapping list_list_chars in
    let (tthm, list_list_chars) = parse_mapping list_list_chars in
    let (htlm, list_list_chars) = parse_mapping list_list_chars in
    let loc = apply_map_ranges stsm seed_range
            |> apply_map_ranges stfm
            |> apply_map_ranges ftwm
            |> apply_map_ranges wtlm
            |> apply_map_ranges lttm
            |> apply_map_ranges tthm
            |> apply_map_ranges htlm in
    let (start_ranges, end_ranges) = List.unzip loc in
    Set.min_elt_exn (Set.of_list (module Int) start_ranges)
