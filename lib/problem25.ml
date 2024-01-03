open Core

type component = {name: string; wires: string list; nnodes: int}

let rec get_comp_and_wires comp connections list_strings = 
    match list_strings with
    | [] -> 
            let comp = List.sort ~compare:String.compare comp |> List.remove_consecutive_duplicates ~equal:String.equal in
            let comp_recs = List.map ~f:(fun x -> {name=x; wires=[]; nnodes=1}) comp in
            (comp_recs, connections)
    | h :: t ->
            let [compi; wired] = String.split h ~on:':' |> List.map ~f:String.strip in
            let wired = String.split wired ~on:' ' in
            let connectionsi = List.cartesian_product [compi] wired in
            let compi = compi :: wired in
            get_comp_and_wires (compi @ comp) (connectionsi @ connections) t

let rec determine_set dict set ids_check =
    match ids_check with
    | [] -> set
    | id :: ids_tail ->
            if List.mem ~equal:String.equal set id then
                determine_set dict set ids_tail
            else
                let comp = Hashtbl.find_exn dict id in
                let neighbors = comp.wires in
                let ids_check_new = neighbors @ ids_tail in
                determine_set dict (id :: set) ids_check_new

let determine_set2 dict set id_start = determine_set dict set [id_start]

let unwire dict id_from id_to = 
    let comp_from = Hashtbl.find_exn dict id_from in
    let comp_to = Hashtbl.find_exn dict id_to in

    let rec uw_aux head tail id = 
        match tail with
        | [] -> head @ tail
        | h :: t ->
                if String.equal h id then
                    head @ t
                else
                    uw_aux (h :: head) t id
    in
    let wires_from_new = uw_aux [] comp_from.wires id_to in
    let wires_to_new = uw_aux [] comp_to.wires id_from in
   
    let comp_from_new = {comp_from with wires=wires_from_new} in
    let comp_to_new = {comp_to with wires=wires_to_new} in

    Hashtbl.set dict ~key:id_from ~data:comp_from_new;
    Hashtbl.set dict ~key:id_to ~data:comp_to_new;
    ()

let wire_from_to dict id_from id_to = 
    let comp_from = Hashtbl.find_exn dict id_from in
    let comp_to = Hashtbl.find_exn dict id_to in

    let comp_from_new = {comp_from with wires=(id_to :: comp_from.wires)} in
    let comp_to_new = {comp_to with wires=(id_from :: comp_to.wires)} in

    Hashtbl.set dict ~key:id_from ~data:comp_from_new;
    Hashtbl.set dict ~key:id_to ~data:comp_to_new;
    ()

let rec remove_wires dict connections =
    match connections with
    | [] -> ()
    | (id_from, id_to) :: t ->
            let _ = unwire dict id_from id_to in
            remove_wires dict t

let rec wire_diagram dict connections = 
    match connections with
    | [] -> ()
    | (id_from, id_to) :: t -> 
            let _ = wire_from_to dict id_from id_to in
            wire_diagram dict t

let remove_and_calc_nsets dict threewires = 
    remove_wires dict threewires;
    let (first, second) = List.unzip threewires in
    
    let all_ids = List.concat [first; second]
                |> List.sort ~compare:String.compare
                |> List.remove_consecutive_duplicates ~equal:String.equal
    in

    let sets = List.map ~f:(determine_set2 dict []) all_ids in

    let sets = List.map ~f:(List.sort ~compare:String.compare) sets in

    let compare_aux list1 list2 = 
        let (n1, n2) = (List.length list1, List.length list2) in
        Int.compare n1 n2
    in
    let sets = List.sort ~compare:compare_aux sets in

    let sets = List.remove_consecutive_duplicates ~equal:(List.equal String.equal) sets in
    
    wire_diagram dict threewires;

    let length_each_set = List.map ~f:List.length sets in
    let prod_val = List.reduce ~f:( * ) length_each_set in

    (List.length sets, prod_val)

let rec determine_wires dict three_wire_combs = 
    match three_wire_combs with
    | [] -> None
    | h :: t ->
            let (nsets, prod) = remove_and_calc_nsets dict h in
            if nsets = 2 then
                prod
            else
                determine_wires dict t

let rec replace_in_list head string_list string_old string_new = 
    match string_list with
    | [] -> head
    | h :: t -> 
            if List.mem ~equal:String.equal h string_old then
                (string_new :: head) @ t
            else
                replace_in_list (h :: head) t string_old string_new

let rec update_edges head edges string_new string1_old string2_old =
    (* We want to replace any instances of the old node name with the new node name and remove the edge that contains both nodes *)
    match edges with
    | [] -> head
    | (str1, str2) :: t ->
            if (String.equal string1_old str1 && String.equal string2_old str2) || (String.equal string1_old str2 && String.equal string2_old str1) then
                (* Drop the edge between the two contracted nodes *)
                update_edges head t string_new string1_old string2_old
            else if String.equal string1_old str1 || String.equal string2_old str1 then
                (* Replace the first node in the edge with the new name *)
                update_edges ((string_new, str2) :: head) t string_new string1_old string2_old
            else if String.equal string1_old str2 || String.equal string2_old str2 then
                (* Replace the second node in the edge with the new name *)
                update_edges ((str1, string_new) :: head) t string_new string1_old string2_old
            else
                (* Continue with no modifications to the edge *)
                update_edges ((str1, str2) :: head) t string_new string1_old string2_old

let contract_nodes dict edges node1 node2 =
    let node_new_id = String.concat [node1; node2] in
    let num1 = Hashtbl.find_exn dict node1 in
    let num2 = Hashtbl.find_exn dict node2 in

    (* Replace the 2 nodes that the edge contracted *)
    Hashtbl.remove dict node1;
    Hashtbl.remove dict node2;
    Hashtbl.set dict ~key:node_new_id ~data:(num1+num2);

    (* Replace the old node names with the new contracted name in the list of edges *)
    update_edges [] edges node_new_id node1 node2

let rec karger ntimes dict edges = 
    (* If we aren't running the contraction anymore return the edges. Otherwise, apply a contraction of a random edge and run the function again *)
    if ntimes = 0 then
        edges
    else
        (* Choose a random edge and contract it *)
        let (node1, node2) = List.length edges
                           |> Random.int
                           |> List.nth_exn edges
        in

        let new_edges = contract_nodes dict edges node1 node2 in
        karger (ntimes-1) dict new_edges

let rec find_threecut dict edges = 
    let keys = Hashtbl.keys dict in
    let new_dict = Hashtbl.create ~size:(List.length keys) (module String) in

    let nd_aux nd name = 
        Hashtbl.set nd ~key:name ~data:1 in
    let _ = List.map ~f:(nd_aux new_dict) keys in

    let nkeys = List.length keys in
    let new_edges = karger (nkeys-2) new_dict edges in

    if List.length new_edges = 3 then
        let keys_new = Hashtbl.keys new_dict in
        let num_nodes = List.map ~f:(fun x -> (String.length x)/3) keys_new in
        List.reduce ~f:( * ) num_nodes
    else
        find_threecut dict edges

let part1 filename = 
    let list_strings = Util.parse_to_lines filename in
    let (comp, connections) = get_comp_and_wires [] [] list_strings in
    let dict = Hashtbl.create ~size:(List.length comp) (module String) in
    let _ = List.map ~f:(fun x -> Hashtbl.set dict ~key:x.name ~data:x) comp in
    wire_diagram dict connections;
    find_threecut dict connections
