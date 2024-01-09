open Core

type lens_op = {lens: string; value: int; hash: int}

let rec get_lens_values list_lens = 
    match list_lens with
    | [] -> []
    | h :: t -> h.value :: get_lens_values t

let calc_focusing_power boxes = 
    let cfp_aux list_lens = 
        let lens_values = get_lens_values list_lens in
        let tmp = List.reduce ~f:(+) (List.mapi ~f:(fun i x -> (i+1)*x) lens_values) in
        match tmp with
        | Some result -> result
        | None -> 0
    in   
    let tmp = List.mapi ~f:(fun i x -> (i+1)*(cfp_aux x)) boxes in
    List.reduce_exn ~f:(+) tmp

let calc_hash list_chars = 
    let rec ch_aux num list_chars = 
        match list_chars with
        | [] -> num
        | h :: t ->
                let num = Int.rem ((num + Char.to_int h)*17) 256 in
                ch_aux num t
    in
    ch_aux 0 list_chars

let calc_hash_str str = calc_hash (String.to_list str)

let split_by_sep sep list_chars =
    let rec sbs_aux llc lc_tmp list_chars = 
        match list_chars with
        | [] -> List.rev ((List.rev lc_tmp) :: llc)
        | h :: t when Char.compare h sep = 0 ->
                sbs_aux ((List.rev lc_tmp) :: llc) [] t
        | h :: t -> sbs_aux llc (h :: lc_tmp) t
    in
    sbs_aux [] [] list_chars

let apply_lens_op lens_list lop = 
    let rec alop_aux list_head list_tail lop =
        match list_tail with
        | [] -> 
                if lop.value = -1 then
                    List.rev list_head
                else
                    List.rev (lop :: list_head)
        | h :: t when String.compare h.lens lop.lens = 0 ->
                if lop.value = -1 then
                    List.append (List.rev list_head) t
                else
                    List.append (List.rev (lop :: list_head)) t
        | h :: t -> alop_aux (h :: list_head) t lop
    in
    alop_aux [] lens_list lop

let construct_hashmap lensops =
    let boxes = Array.create ~len:256 [] in
    let rec ch_aux lensops =
        match lensops with
        | [] -> 0
        | h :: t -> 
                let i = h.hash in
                let lens_list = boxes.(i) in
                boxes.(i) <- (apply_lens_op lens_list h);
                ch_aux t

    in
    ch_aux lensops;
    boxes

let construct_lens_op list_chars = 
    let rec clo_aux lname list_chars = 
        match list_chars with
        | [] -> (String.of_char_list (List.rev lname), -1)
        | h :: t when Char.compare h '=' = 0 ->
                (String.of_char_list (List.rev lname), Util.parse_num t)
        | h :: t when Char.compare h '-' = 0 ->
                (String.of_char_list (List.rev lname), -1)
        | h :: t -> clo_aux (h :: lname) t
    in
    let (lname, lvalue) = clo_aux [] list_chars in
    let hash = calc_hash_str lname in
    {lens=lname; value=lvalue; hash=hash}
    
let part1 filename = 
    let [list_chars] = Util.parse_to_list_list_chars filename in
    let list_list_chars = split_by_sep ',' list_chars in
    let hash_list = List.map ~f:calc_hash list_list_chars in
    List.reduce_exn ~f:(+) hash_list

let part2 filename = 
    let [list_chars] = Util.parse_to_list_list_chars filename in
    let list_list_chars = split_by_sep ',' list_chars in
    let lens_op_list = List.map ~f:construct_lens_op list_list_chars in
    let boxes = construct_hashmap lens_op_list in
    calc_focusing_power (Array.to_list boxes)
