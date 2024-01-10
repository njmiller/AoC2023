open Core

let get_big_idxs dimi dimj i j = 
    let i2 = Int.rem i dimi in
    let i2 = if i2 < 0 then i2 + dimi else i2 in
    let j2 = Int.rem j dimj in
    let j2 = if j2 < 0 then j2 + dimj else j2 in
    
    let ki_tmp = if i < (-3*dimi) then
                     0
                 else if i < (-2*dimi) then
                     1
                 else if i < (-dimi) then
                     2
                 else if i < 0 then
                     3
                 else if i < dimi then
                     4
                 else if i < (2*dimi) then
                     5
                 else if i < (3*dimi) then
                     6
                 else if i < (4*dimi) then
                     7
                 else
                     8
    in
    
    let kj_tmp = if j < (-3*dimj) then
                     0
                 else if j < (-2*dimj) then
                     1
                 else if j < (-dimj) then
                     2
                 else if j < 0 then
                     3
                 else if j < dimj then
                     4
                 else if j < (2*dimj) then
                     5
                 else if j < (3*dimj) then
                     6
                 else if j < (4*dimj) then
                     7
                 else
                     8
    in

    (*let ki_tmp = if i < -dimi then
                     0
                 else if i < 0 then
                     1
                 else if i < dimi then
                     2
                 else if i < (2*dimi) then
                     3
                 else
                     4
    in

    let kj_tmp = if j < -dimj then
                    0
                else if j < 0 then
                    1
                else if j < dimj then
                    2
                else if j < (2*dimj) then
                    3
                else
                    4
    in*)

    let k2 = ki_tmp + 9*kj_tmp in

    (i2, j2, k2)

let get_adjacent_big aa_chars unvisited (i, j) =
    let dimi = Bigarray.Array3.dim1 unvisited in
    let dimj = Bigarray.Array3.dim2 unvisited in
    let nodes = [(i+1, j); (i-1, j); (i, j+1); (i, j-1)] in

    let filter_func (i, j) =
        let (i2, j2, k2) = get_big_idxs dimi dimj i j in
        unvisited.{i2, j2, k2} = 1
    in
    let nodes = List.filter ~f:filter_func nodes in

    let filter_func2 (i, j) = 
        let (i2, j2, k2) = get_big_idxs dimi dimj i j in
        Char.compare aa_chars.(i2).(j2) '#' <> 0
    in
    let nodes = List.filter ~f:filter_func2 nodes in

    nodes

let get_adjacent aa_chars unvisited (i, j) =
    let (dimi, dimj) = (Array.length aa_chars, Array.length aa_chars.(0)) in
    let nodes = [(i+1, j); (i-1, j); (i, j+1); (i, j-1)] in
    let nodes = List.filter ~f:(fun (x, y) -> x >= 0 && x < dimi && y >= 0 && y < dimj) nodes in
    let nodes = List.filter ~f:(fun (x, y) -> unvisited.(x).(y)) nodes in
    let nodes = List.filter ~f:(fun (x, y) -> Char.compare aa_chars.(x).(y) '#' <> 0) nodes in
    nodes

let rec queue_all_nodes queue nodes = 
    match nodes with
    | [] -> ()
    | h :: t ->
            let _ = Queue.enqueue queue h in
            queue_all_nodes queue t

let rec get_next_node_big unvisited queue = 
    let dimi = Bigarray.Array3.dim1 unvisited in
    let dimj = Bigarray.Array3.dim2 unvisited in
    let res = Queue.dequeue queue in
    match res with
    | Some (x, y, nsteps) -> 
            let (i2, j2, k2) = get_big_idxs dimi dimj x y in
            if unvisited.{i2, j2, k2} = 1 then
                res
            else
                get_next_node_big unvisited queue
    | None -> None

let rec get_next_node unvisited queue = 
    let res = Queue.dequeue queue in
    match res with
    | Some (x, y, nsteps) -> 
            if unvisited.(x).(y) then
                res
            else
                get_next_node unvisited queue
    | None -> None

let rec bfs_big npos maxsteps unvisited queue aa_chars (i, j, nsteps) = 
    
    (* Mark location as visited so we don't come to it again and increment the possible locations if the number of steps is even *) 
    let dimi = Bigarray.Array3.dim1 unvisited in
    let dimj = Bigarray.Array3.dim2 unvisited in
    let (i2, j2, k2) = get_big_idxs dimi dimj i j in
    unvisited.{i2, j2, k2} <- 0;
    let npos = if (Int.rem nsteps 2) = (Int.rem maxsteps 2) then npos+1 else npos in

    (* Find all the adjacent nodes only if we haven't reached the maximum number of steps *)
    let nodes_adj = 
        if nsteps <= maxsteps then 
            get_adjacent_big aa_chars unvisited (i, j)
        else
            []
    in

    (*let nodes_adj = List.filter (fun x -> nsteps <= maxsteps) nodes_adj in*)
    let nodes_queue = List.map ~f:(fun (x, y) -> (x, y, nsteps+1)) nodes_adj in

    (* Add to the queue *)
    queue_all_nodes queue nodes_queue;

    (* Get the next node off off the queue *)
    let next = get_next_node_big unvisited queue in

    (* If there is nothing in the queue, return, otherwise recusively call bfs *)
    match next with
    | Some res ->
            bfs_big npos maxsteps unvisited queue aa_chars res
    | None -> npos

let rec bfs npos maxsteps unvisited queue aa_chars (i, j, nsteps) = 
    
    (* Mark location as visited so we don't come to it again and increment the possible locations if the number of steps is even *) 
    unvisited.(i).(j) <- false;
    let npos = if Int.rem nsteps 2 = 0 then npos+1 else npos in

    (* Find all the adjacent nodes only if we haven't reached the maximum number of steps *)
    let nodes_adj = 
        if nsteps <= maxsteps then 
            get_adjacent aa_chars unvisited (i, j)
        else
            []
    in

    (*let nodes_adj = List.filter (fun x -> nsteps <= maxsteps) nodes_adj in*)
    let nodes_queue = List.map ~f:(fun (x, y) -> (x, y, nsteps+1)) nodes_adj in

    (* Add to the queue *)
    queue_all_nodes queue nodes_queue;

    (* Get the next node off off the queue *)
    let next = get_next_node unvisited queue in

    (* If there is nothing in the queue, return, otherwise recusively call bfs *)
    match next with
    | Some res ->
            bfs npos maxsteps unvisited queue aa_chars res
    | None -> npos

let part1 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let aa_chars = Array.of_list_map ~f:Array.of_list list_list_chars in
    let queue = Queue.create ~capacity:100 () in
    let unvisited = Array.make_matrix ~dimx:131 ~dimy:131 true in
    bfs 0 64 unvisited queue aa_chars (65, 65, 0)
    (*let unvisited = Array.make_matrix ~dimx:11 ~dimy:11 true in
    bfs 0 6 unvisited queue aa_chars (5, 5, 0)*)

let part2 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let aa_chars = Array.of_list_map ~f:Array.of_list list_list_chars in
    let queue = Queue.create ~capacity:1000 () in
    let (dimi, dimj) = (131, 131) in
    let unvisited = Bigarray.Array3.create Bigarray.int Bigarray.c_layout dimi dimj 81 in
    Bigarray.Array3.fill unvisited 1;
    let nsteps65 = bfs_big 0 65 unvisited queue aa_chars (65, 65, 0) in
    Bigarray.Array3.fill unvisited 1;
    let nsteps327 = bfs_big 0 327 unvisited queue aa_chars (65, 65, 0) in
    Bigarray.Array3.fill unvisited 1;
    let nsteps589 = bfs_big 0 589 unvisited queue aa_chars (65, 65, 0) in
    (* Take these 3 values, calculate a quadratic from them and then evaluate at the right number of steps *)
    let tmp = (nsteps65, nsteps327, nsteps589) in
    1

    
