open Core

type part = {x: int; m: int; a: int; s: int}
type part_range = {xrange: int*int; mrange: int*int;
                   arange: int*int; srange: int*int}

let get_new_pr pr_old prop range_new = 
    match prop with
    | "a" -> {pr_old with arange=range_new}
    | "x" -> {pr_old with xrange=range_new}
    | "m" -> {pr_old with mrange=range_new}
    | "s" -> {pr_old with srange=range_new}

let rec iterate_rule_list_pr rule_list pr =
    let h :: t = rule_list in
    let (prop, func, propval, res) = h in
    if propval = -9999 then
        [(res, pr)]
    else
        let (minval, maxval) = (match prop with
                                | "a" -> pr.arange 
                                | "x" -> pr.xrange 
                                | "m" -> pr.mrange 
                                | "s" -> pr.srange
                               ) in
        let typeres = (if propval >= maxval then
                          1
                      else if propval >= minval then
                          2
                      else
                          3
                      )
        in
        (* > *)
        if func 1 0 then
            (match typeres with
             | 1 -> iterate_rule_list_pr t pr
             | 3 -> [(res, pr)]
             | 2 ->
                     let range1 = (minval, propval) in
                     let range2 = (propval+1, maxval) in
                     let pr_new1 = get_new_pr pr prop range1 in
                     let pr_new2 = get_new_pr pr prop range2 in
                     (res, pr_new2) :: (iterate_rule_list_pr t pr_new1)
            )
        (* < *)
        else
            (match typeres with
             | 3 -> iterate_rule_list_pr t pr
             | 1 -> [(res, pr)]
             | 2 ->
                     let range1 = (minval, propval-1) in
                     let range2 = (propval, maxval) in
                     let pr_new1 = get_new_pr pr prop range1 in
                     let pr_new2 = get_new_pr pr prop range2 in
                     (res, pr_new1) :: (iterate_rule_list_pr t pr_new2)
            )

let rec process_wf_prs dict wf_prs =
    match wf_prs with
    | [] -> []
    | (wf, pr) :: t ->
        if String.compare wf "A" = 0 then
            pr :: (process_wf_prs dict t)
        else if String.compare wf "R" = 0 then
            process_wf_prs dict t
        else
            let rule_list = Hashtbl.find_exn dict wf in
            let list_out = iterate_rule_list_pr rule_list pr in
            process_wf_prs dict (list_out @ t)

let rec iterate_rule_list rule_list part = 
    let h :: t = rule_list in
    let (prop, func, propval, res) = h in
    match prop with 
    | "a" -> if func part.a propval then res else iterate_rule_list t part
    | "x" -> if func part.x propval then res else iterate_rule_list t part
    | "m" -> if func part.m propval then res else iterate_rule_list t part
    | "s" -> if func part.s propval then res else iterate_rule_list t part
    | "" -> res

let rec process_part dict rule_name part =
    let rule_list = Hashtbl.find_exn dict rule_name in
    let res = iterate_rule_list rule_list part in
    match res with
    | "A" -> part.x + part.a + part.m + part.s
    | "R" -> 0
    | _ -> process_part dict res part
    

let rec process_parts dict part_list =
    match part_list with
    | [] -> 0
    | h :: t -> (process_part dict "in" h) + (process_parts dict t)

let rec read_until (chars: char list) (sep: char) (list_chars: char list) =
    match list_chars with
    | h :: t when Char.compare sep h = 0 -> (String.of_char_list (List.rev chars), t, 1)
    | '}' :: t -> (String.of_char_list (List.rev chars), [], 0)
    | h :: t -> read_until (h :: chars) sep t

let rec pwf_aux rule_list list_chars = 
    let (rule, list_chars, suc) = read_until [] ',' list_chars in
    if suc = 1 then
        let [prop; propval; res] = String.split_on_chars rule ['<'; '>'; ':'] in
        let func = if (String.contains rule '>') then (>) else (<) in
        let rule_tmp = (prop, func, Int.of_string propval, res) in
        pwf_aux (rule_tmp :: rule_list) list_chars
    else
        List.rev (("", (=), -9999, rule) :: rule_list)

let parse_workflow dict list_chars = 
    let (name, list_chars, suc) = read_until [] '{' list_chars in
    let rule_list = pwf_aux [] list_chars in
    Hashtbl.set dict ~key:name ~data:rule_list
    
let parse_part list_chars = 
    let h :: list_chars = list_chars in
    let (xdata, list_chars, suc) = read_until [] ',' list_chars in
    let (mdata, list_chars, suc) = read_until [] ',' list_chars in
    let (adata, list_chars, suc) = read_until [] ',' list_chars in 
    let (sdata, list_chars, suc) = read_until [] ',' list_chars in
    let [xstr; xval] = String.split_on_chars xdata ['='] in
    let [mstr; mval] = String.split_on_chars mdata ['='] in
    let [astr; aval] = String.split_on_chars adata ['='] in
    let [sstr; sval] = String.split_on_chars sdata ['='] in
    {x=Int.of_string xval; m=Int.of_string mval; a=Int.of_string aval; s=Int.of_string sval}

let rec parse_parts list_list_chars =
    match list_list_chars with
    | [] -> []
    | h :: t -> parse_part h :: parse_parts t

let rec parse_to_workflows_parts dict list_list_chars = 
    match list_list_chars with
    | [] :: t -> (dict, parse_parts t)
    | h :: t ->
            parse_workflow dict h;
            parse_to_workflows_parts dict t

let rec get_number_comb prs = 
    match prs with
    | [] -> 0
    | h :: t ->
            let (a1, a2) = h.arange in
            let (x1, x2) = h.xrange in
            let (m1, m2) = h.mrange in
            let (s1, s2) = h.srange in
            (a2-a1+1)*(x2-x1+1)*(m2-m1+1)*(s2-s1+1) + get_number_comb t

let part1 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let dict = Hashtbl.create (module String) in
    let (dict, parts) = parse_to_workflows_parts dict list_list_chars in
    process_parts dict parts

let part2 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let dict = Hashtbl.create (module String) in
    let (dict, parts) = parse_to_workflows_parts dict list_list_chars in
    let pr1 = {xrange=(1,4000); mrange=(1,4000); arange=(1,4000);
               srange=(1,4000)} in
    let wf_pr1 = ("in", pr1) in
    let pr_accept = process_wf_prs dict [wf_pr1] in
    get_number_comb pr_accept
    
