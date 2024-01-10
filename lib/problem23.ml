open Core


(* ID should be array index. Neighbors list ID and distance of neighbors and location lists the i,j location in the original map *)
type node = {id: string; neighbors: (string*int) list; location: int*int; unvisited: bool}

let get_adjacent2 (i, j) unvisited aa_chars = 
    let (dimi, dimj) = (Array.length aa_chars, Array.length aa_chars.(0)) in

    let nodes2 = [(i+1, j); (i-1, j); (i, j+1); (i, j-1)] in
    let nodes2 = List.filter ~f:(fun (x, y) -> x >= 0 && y >= 0 && x < dimi && y < dimj) nodes2 in 
    let nodes2 = List.filter ~f:(fun (x, y) -> not (Char.equal '#' aa_chars.(x).(y))) nodes2 in

    (*let nodes2 = List.filter ~f:(fun (x, y) -> unvisited.(x).(y)) nodes2 in*)
    if Char.equal '#' aa_chars.(i).(j) then
        []
    else
        nodes2

let get_adjacent (i, j) unvisited aa_chars = 
    let (dimi, dimj) = (Array.length aa_chars, Array.length aa_chars.(0)) in

    match aa_chars.(i).(j) with
    | '>' -> if unvisited.(i).(j+1) then [(i, j+1)] else []
    | '<' -> if unvisited.(i).(j-1) then [(i, j-1)] else []
    | 'v' -> if unvisited.(i+1).(j) then [(i+1, j)] else []
    | '^' -> if unvisited.(i-1).(j) then [(i-1, j)] else []
    | '.' -> 
            let nodes2 = [(i+1, j); (i-1, j); (i, j+1); (i, j-1)] in
            let nodes2 = List.filter ~f:(fun (x, y) -> x >= 0 && y >= 0 && x < dimi && y < dimj) nodes2 in 
            let nodes2 = List.filter ~f:(fun (x, y) -> not (Char.equal '#' aa_chars.(x).(y))) nodes2 in
            List.filter ~f:(fun (x, y) -> unvisited.(x).(y)) nodes2
    | _ -> []

let rec flp_aux nsteps (ie, je) aa_chars unvisited (i, j) = 
   
    unvisited.(i).(j) <- false;

    let nodes = get_adjacent (i, j) unvisited aa_chars in

    if i=ie && j=je then
        let _ = unvisited.(i).(j) <- true in
        nsteps
    else if List.length nodes = 0 then
        let _ = unvisited.(i).(j) <- true in
        -1
    else
        let max_distances = List.map ~f:(flp_aux (nsteps+1) (ie, je) aa_chars unvisited) nodes in
        let _ = unvisited.(i).(j) <- true in
        (*List.filter ~f:(fun x -> x > 0) (List.concat max_distances)*)
        (*List.concat max_distances*)
        let max_dist = List.max_elt ~compare:Int.compare max_distances in
        match max_dist with
        | Some res -> 
                res
        | None -> -1
(*
let simplify nsteps (ie, je) aa_chars unvisited (i, j) = 
    
    unvisited.(i).(j) <- false;

    let nodes = get_adjacent2 (i, j) unvisited aa_chars in

    if i=ie && j=je then
        let _ = unvisited.(i).(j) <- true in
        [nsteps]
    else if List.length nodes = 0 then
        let _ = unvisited.(i).(j) <- true in
        [-1]
    else
        let max_distances = List.map ~f:(flp_aux (nsteps+1) (ie, je) aa_chars unvisited) nodes in
        let _ = unvisited.(i).(j) <- true in
        (*List.filter ~f:(fun x -> x > 0) (List.concat max_distances)*)
        List.concat max_distances
        (*let max_dist = List.max_elt ~compare:Int.compare max_distances in
        match max_dist with
        | Some res -> res
        | None -> -1*)
*)

let find_longest_path (i0, j0) (i1, j1) aa_chars = 
    let (dimi, dimj) = (Array.length aa_chars, Array.length aa_chars.(0)) in
    let unvisited = Array.make_matrix ~dimx:dimi ~dimy:dimj true in
    let all_distances = flp_aux 0 (i1, j1) aa_chars unvisited (i0, j0) in
    (*List.filter ~f:(fun x -> x >= 0) all_distances*)
    all_distances

(*
let simplify_graph aa_chars = 
    let (dimi, dimj) = (Array.length aa_chars, Array.length aa_chars.(0)) in
    
    let rec find_intersections intersections (i, j) aa_chars = 
        let unvisited = Array.make_matrix ~dimx:dimi ~dimy:dimj true in
        let nodes = get_adjacent2 (i, j) unvisited aa_chars in
        let intersections = if List.length nodes > 2 then (i,j) :: intersections else intersections in
        
        let j = if i = (dimi-1) then j+1 else j in
        let i = if i = (dimi-1) then 0 else i+1 in

        if j = dimj then
            intersections
        else
            find_intersections intersections (i, j) aa_chars
    in

    let intersections = find_intersections [] (0, 0) aa_chars in
    (*let intersections = map_intersections intersections in*)
    let intersections = (0, 1) :: (dimi-1, dimj-2) :: intersections in
    let nintersections = List.length intersections in
    let ids = 0--nintersections in
    let intersections = List.zip_exn intersections ids in
    let node_list = List.map ~f:(fun ((i, j), id) -> {id=id; neighbors=[]; location=(i,j); unvisited=true}) intersections in
    let node_array = Array.of_list node_list in
    let node_array = map_intersections node_array in
    node_array
*)

let update_intersections node_dict node_from nsteps (i, j) = 
    let node_name = String.concat [Int.to_string i; " "; Int.to_string j] in
    let node_prev = Hashtbl.find_exn node_dict node_from in
    let node_prev_new = {node_prev with neighbors=(node_name, nsteps) :: node_prev.neighbors} in
    let node_new_opt = Hashtbl.find node_dict node_name in
    let node_new = (match node_new_opt with
                    | Some res -> {res with neighbors=(node_prev.id, nsteps) :: res.neighbors}
                    | None -> {id=node_name; neighbors=[(node_prev.id, nsteps)]; location=(i, j); unvisited=true}
                   ) in
    let _ = Hashtbl.set node_dict ~key:node_prev.id ~data:node_prev_new in
    let _ = Hashtbl.set node_dict ~key:node_new.id ~data:node_new in
    ()

let not_prev_int node_from (i, j) =
    let strs = String.split node_from ' ' in
    let [i0; j0] = List.map ~f:Int.of_string strs in
    not (i = i0 && j = j0)

let rec walk_full_map node_dict node_from nsteps unvisited aa_chars (i, j) = 
    unvisited.(i).(j) <- false;
    let nodes = get_adjacent2 (i, j) unvisited aa_chars in

    (* An intersection is where there are more than 2 (forward, backward) possible adjacent nodes *)
    if List.length nodes > 2 then
        let _ = unvisited.(i).(j) <- true in
        let node_name = String.concat [Int.to_string i; " "; Int.to_string j] in
        let _ = update_intersections node_dict node_from nsteps (i, j) in 
        
        let nodes = List.filter ~f:(fun (x, y) -> unvisited.(x).(y)) nodes in
        let _ = List.map ~f:(walk_full_map node_dict node_name 1 unvisited aa_chars) nodes in
        ()
    else if List.length nodes = 1 then
        let (dimi, dimj) = (Array.length aa_chars, Array.length aa_chars.(0)) in
        if i = (dimi-1) && j = (dimj-2) then
            let _ = update_intersections node_dict node_from nsteps (i, j) in
            ()
        else if i = 0 && j = 1 then
            let [node] = nodes in
            walk_full_map node_dict node_from (nsteps+1) unvisited aa_chars node
        else
            ()
    else
        let nodes = List.filter ~f:(fun (x, y) -> unvisited.(x).(y)) nodes in
        let nodes = List.filter ~f:(fun (x, y) -> not_prev_int node_from (x, y)) nodes in
        if List.length nodes > 0 then
            let [node] = nodes in
            walk_full_map node_dict node_from (nsteps+1) unvisited aa_chars node
        else
            ()

let simplify_graph2 aa_chars = 
    let (dimi, dimj) = (Array.length aa_chars, Array.length aa_chars.(0)) in
    let unvisited = Array.make_matrix ~dimx:dimi ~dimy:dimj true in
    let node1 = {id="0 1"; neighbors=[]; unvisited=true; location=(0, 1)} in
    let node_dict = Hashtbl.create (module String) in
    Hashtbl.set node_dict ~key:"0 1" ~data:node1;
    walk_full_map node_dict "0 1" 0 unvisited aa_chars (0, 1);
    node_dict

let rec lp_simp_aux i1 graph (id, nsteps) =

    let node_curr = Hashtbl.find_exn graph id in
    let node_curr_up = {node_curr with unvisited=false} in
    Hashtbl.set graph ~key:id ~data:node_curr_up;

    let nodes = node_curr.neighbors in
    
    let filter_func (id, n) =
        let node = Hashtbl.find_exn graph id in
        node.unvisited
    in
    let nodes = List.filter ~f:filter_func nodes in

    (* Add the total number of steps to the number of steps to the next node *)
    let nodes = List.map ~f:(fun (x, y) -> (x, y+nsteps)) nodes in
    
    if String.equal id i1 then
        let _ = Hashtbl.set graph ~key:id ~data:node_curr in
        nsteps
    else if List.length nodes = 0 then
        let _ = Hashtbl.set graph ~key:id ~data:node_curr in
        -1
    else
        let max_distances = List.map ~f:(lp_simp_aux i1 graph) nodes in
        let _ = Hashtbl.set graph ~key:id ~data:node_curr in
        (*List.filter ~f:(fun x -> x > 0) (List.concat max_distances)*)
        (*List.concat max_distances*)
        let max_dist = List.max_elt ~compare:Int.compare max_distances in
        match max_dist with
        | Some res -> 
                res
        | None -> -1

let find_lp_simplified id0 id1 graph = 
    lp_simp_aux id1 graph (id0, 0)

let part1 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let aa_chars = Array.of_list_map ~f:Array.of_list list_list_chars in
    let (dimi, dimj) = (Array.length aa_chars, Array.length aa_chars.(0)) in
    let paths = find_longest_path (0, 1) (dimi-1, dimj-2) aa_chars in
    (*List.max_elt ~compare:Int.compare paths*)
    paths

let part2 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let aa_chars = Array.of_list_map ~f:Array.of_list list_list_chars in
    let (dimi, dimj) = (Array.length aa_chars, Array.length aa_chars.(0)) in
    let graph = simplify_graph2 aa_chars in
    (*let paths = find_longest_path (0, 1) (dimi-1, dimj-2) graph in
    paths*)
    (*Hashtbl.data graph*)
    let paths = find_lp_simplified "0 1" "140 139" graph in
    paths
    (*Hashtbl.find graph "0 1"*)
