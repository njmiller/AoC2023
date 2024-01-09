open Core

let rec print_list_nums nums = 
    match nums with
    | [] -> let _ = Out_channel.output_char stdout '\n' in
            0
    | h :: t -> let _ = Out_channel.output_string stdout (Int.to_string h) in
                let _ = Out_channel.output_char stdout ' ' in
                print_list_nums t

let rec print_list_chars chars = 
    match chars with
    | [] -> let _ = Out_channel.output_char stdout ' ' in
            0
    | h :: t -> let _ = Out_channel.output_char stdout h in
                print_list_chars t

let rec nums_to_string str_out nums = 
    match nums with
    | [] -> str_out
    | h :: t -> let num_str = Int.to_string h in
                let tmp1 = String.append num_str "," in
                nums_to_string (String.append str_out tmp1) t
                

let data_to_string cons list_chars nums =
    let cons_string = Int.to_string cons in
    let rest = String.of_char_list list_chars in
    let nums_str = nums_to_string "" nums in
    let tmp1 = String.append cons_string rest in
    String.append tmp1 nums_str

let cna_aux_v2 cons ndl list_chars nums = 
    let memot = Hashtbl.create (module String) in
    let rec cna_aux_tmp cons ndl list_chars nums = 
        let strval = data_to_string cons list_chars nums in
        match Hashtbl.find memot strval with
        | Some result -> 
                result
        | None ->
            match list_chars with
                | [] -> 
                    if List.length nums = 1 then
                        let [num] = nums in
                        if cons = num then 
                            let result=1 in
                            Hashtbl.set memot ~key:strval ~data:result;
                            result
                        else 
                            let result=0 in
                            Hashtbl.set memot ~key:strval ~data:result;
                            result
                    else
                        let result=0 in
                        Hashtbl.set memot ~key:strval ~data:result;
                        result
                | '.' :: t -> 
                    let n0 :: n1 = nums in
                    if cons > 0 && cons = n0 then
                        if List.length n1 = 0 && ndl = 0 then 
                            let result=1 in
                            Hashtbl.set memot ~key:strval ~data:result;
                            result
                        else if List.length n1 = 0 then 
                            let result=0 in
                            Hashtbl.set memot ~key:strval ~data:result;
                            result
                    else 
                        let result=cna_aux_tmp 0 ndl t n1 in
                        Hashtbl.set memot ~key:strval ~data:result;
                        result
                    else if cons > 0 then
                        let result=0 in
                        Hashtbl.set memot ~key:strval ~data:result;
                        result
                    else
                        let result=cna_aux_tmp 0 ndl t nums in
                        Hashtbl.set memot ~key:strval ~data:result;
                        result
                | '#' :: t -> 
                    (*let _ = print_list_chars t in
                    let _ = print_list_nums nums in*)
                    let result = cna_aux_tmp (cons+1) (ndl-1) t nums in
                    Hashtbl.set memot ~key:strval ~data:result;
                    result
                | '?' :: t -> 
                    (*let _ = print_list_chars t in
                    let _ = print_list_nums nums in*)
                    let n0 :: n1 = nums in
                    let ifg = if cons > 0 && cons = n0 then
                        if List.length n1 = 0 && ndl = 0 then
                            1
                        else if List.length n1 = 0 then
                            0 
                        else 
                            cna_aux_tmp 0 ndl t n1
                    else if cons > 0 then 
                        0
                    else
                        cna_aux_tmp 0 ndl t nums
                    in
                    let result = (cna_aux_tmp (cons+1) ndl t nums) + ifg in
                    Hashtbl.set memot ~key:strval ~data:result;
                    result
    in
    cna_aux_tmp cons ndl list_chars nums

let rec cna_aux cons ndl list_chars nums =
    match list_chars with
    | [] -> if List.length nums = 1 then
                let [num] = nums in
                if cons = num then 1 else 0
            else
                0
    | '.' :: t -> let n0 :: n1 = nums in
                  if cons > 0 && cons = n0 then
                      if List.length n1 = 0 && ndl = 0 then 1
                      else if List.length n1 = 0 then 0
                      else cna_aux 0 ndl t n1
                  else if cons > 0 then
                      0
                  else
                      cna_aux 0 ndl t nums
    | '#' :: t -> (*let _ = print_list_chars t in
                  let _ = print_list_nums nums in*)
                  cna_aux (cons+1) (ndl-1) t nums
    | '?' :: t -> (*let _ = print_list_chars t in
                  let _ = print_list_nums nums in*)
                  let n0 :: n1 = nums in
                  let ifg = if cons > 0 && cons = n0 then
                                if List.length n1 = 0 && ndl = 0 then 1
                                else if List.length n1 = 0 then 0
                                else cna_aux 0 ndl t n1
                            else if cons > 0 then 0
                            else
                                cna_aux 0 ndl t nums
                  in
                  (cna_aux (cons+1) ndl t nums) + ifg

let calc_num_arrange_v2 data = 
    let (chars, nd, nums) = data in
    cna_aux_v2 0 nd chars nums

let calc_num_arrange data = 
    let (chars, nd, nums) = data in
    cna_aux 0 nd chars nums
    
let unfold_data data = 
    let (chars, nd, nums) = data in
    let nd_new = 5*nd in
    let x = ['?'] in
    let chars_new = chars @ x @ chars @ x @ chars @ x @ chars @ x @ chars in
    let nums_new = nums @ nums @ nums @ nums @ nums in
    (chars_new, nd_new, nums_new)

let rec parse_chars_11_aux nd lc list_chars = 
    match list_chars with
    | ' ' :: t -> (List.rev lc, nd, Util.parse_nums ',' t)
    | '#' :: t -> parse_chars_11_aux (nd+1) ('#' :: lc) t
    | h :: t -> parse_chars_11_aux nd (h :: lc) t

let parse_line_11 list_chars = parse_chars_11_aux 0 [] list_chars


let part1 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let data = List.map ~f:parse_line_11 list_list_chars in
    List.reduce_exn ~f:(+) (List.map ~f:calc_num_arrange data)
    (*List.map ~f:calc_num_arrange_v2 data*)

let part2 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let data = List.map ~f:parse_line_11 list_list_chars in
    let data_unfold = List.map ~f:unfold_data data in
    List.reduce_exn ~f:(+) (List.map ~f:calc_num_arrange_v2 data_unfold)
