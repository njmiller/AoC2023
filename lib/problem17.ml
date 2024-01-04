(*#require "core_kernel.pairing_heap"*)
open Core

type direction = Up | Down | Left | Right | NoDir

let get_dir_idx dir = 
    match dir with
    | Up -> 0
    | Down -> 1
    | Left -> 2
    | Right -> 3
    (* Because this is only used for the starting node and a valid index is needed to mark as visited for marking some stuff at the beginning of the function for the current node *)
    | NoDir -> 0

let get_possible_nodes_uc unvisited (i,j,dir,ndir) = 
    let dims = Bigarray.Genarray.dims unvisited in
    let dimi = dims.(0) in
    let list1 = (match dir with
                 | Up -> 
                         let samedir = if ndir < 10 then [(i-1, j, Up, ndir+1)] else [] in
                         let otherdir = if ndir >= 4 then [(i, j+1, Right, 1); (i, j-1, Left, 1)] else [] in
                         samedir @ otherdir
                 | Down -> 
                         let samedir = if ndir < 10 then [(i+1, j, Down, ndir+1)] else [] in
                         let otherdir = if ndir >= 4 then [(i, j+1, Right, 1); (i, j-1, Left, 1)] else [] in
                         samedir @ otherdir
                 | Left ->
                         let samedir = if ndir < 10 then [(i, j-1, Left, ndir+1)] else [] in
                         let otherdir = if ndir >= 4 then [(i+1, j, Down, 1); (i-1, j, Up, 1)] else [] in
                         samedir @ otherdir
                 | Right -> 
                         let samedir = if ndir < 10 then [(i, j+1, Right, ndir+1)] else [] in
                         let otherdir = if ndir >= 4 then [(i+1, j, Down, 1); (i-1, j, Up, 1)] else [] in
                         samedir @ otherdir
                 | NoDir -> [(i-1, j, Up, 1); (i+1, j, Down, 1); (i, j+1, Right, 1); (i, j-1, Left, 1)]
    ) in
    let list1 = List.filter ~f:(fun (x,y,dirk,n) -> x >= 0 && x < dimi && y >= 0 && y < dimi) list1 in
    let list1 = List.filter ~f:(fun (x,y,dirk,n) -> unvisited.{x, y, get_dir_idx dirk, (n-1)}=1) list1 in
    list1

let get_possible_nodes unvisited (i,j,dir,ndir) = 
    let dims = Bigarray.Genarray.dims unvisited in
    let dimi = dims.(0) in
    let list1 = (match dir with
                 | Up -> [(i-1, j, Up, ndir+1); (i, j+1, Right, 1); (i, j-1, Left, 1)]
                 | Down -> [(i+1, j, Down, ndir+1); (i, j+1, Right, 1); (i, j-1, Left, 1)]
                 | Left -> [(i, j-1, Left, ndir+1); (i+1, j, Down, 1); (i-1, j, Up, 1)]
                 | Right -> [(i, j+1, Right, ndir+1); (i+1, j, Down, 1); (i-1, j, Up, 1)]
                 | NoDir -> [(i-1, j, Up, 1); (i+1, j, Down, 1); (i, j+1, Right, 1); (i, j-1, Left, 1)]
    ) in
    let list1 = List.filter ~f:(fun (x,y,dirk,n) -> x >= 0 && x < dimi && y >= 0 && y < dimi && n <= 3) list1 in
    let list1 = List.filter ~f:(fun (x,y,dirk,n) -> unvisited.{x, y, get_dir_idx dirk, (n-1)}=1) list1 in
    list1

let rec get_next_node unvisited heapt = 
    let res = Pairing_heap.pop heapt in
    match res with
    | Some result -> 
            let ((x, y, z, a), h) = result in
            let k = get_dir_idx z in
            if unvisited.{x, y, k, (a-1)} = 1 then
                res
            else
                get_next_node unvisited heapt
    | None -> None

let rec add_nodes heapt nodes =
    match nodes with
    | [] -> ()
    | h :: t ->
            Pairing_heap.add heapt h;
            add_nodes heapt t

let rec d_aux node_fn (i1, j1) heapt aa_int path_map unvisited ((i,j,dir,n), h) =
    let k = get_dir_idx dir in

    (* Mark the node as visited and update the path map with the heat value at the current state *)
    unvisited.{i, j, k, (n-1)} <- 0;
    path_map.{i, j, k, (n-1)} <- h;
   
    (* Get possible nodes given the input state. This is filtered by whether we have already visited that state. *)
    let nodes = node_fn unvisited (i,j,dir,n) in
    
    (* Generate a tuple with the node location and heat for this path to this state *)
    let nodes_heat = List.map ~f:(fun (x,y,d,n) -> ((x,y,d,n),h+aa_int.(x).(y))) nodes in

    (* Add the list of nodes to the priority queue *)
    add_nodes heapt nodes_heat;
    
    (* Recursively call the function on the next node in the priority queue. If there are no more nodes in the queue, return from the function. *)
    let node = get_next_node unvisited heapt in
    (*match node with
    | Some result -> d_aux heapt aa_int path_map unvisited result
    | None -> path_map*)
    match node with
    | Some ((x,y,d,n), h) -> 
            if x = i1 && y = j1 then
                h
            else
                d_aux node_fn (i1, j1) heapt aa_int path_map unvisited ((x,y,d,n), h)
    | None -> 0

let dijkstra (i0, j0) (i1, j1) aa_ints = 
    let (dimi, dimj) = (Array.length aa_ints, Array.length aa_ints.(0)) in
    
    (* Generate the 4d arrays for the nodes *)
    let path_map = Bigarray.Genarray.create Bigarray.int Bigarray.c_layout [| dimi; dimj; 4; 3 |] in
    let unvisited = Bigarray.Genarray.create Bigarray.int Bigarray.c_layout [| dimi; dimj; 4; 3 |] in
    Bigarray.Genarray.fill path_map 100000;
    Bigarray.Genarray.fill unvisited 1;
            
    let compare_func (x1, h1) (x2, h2) = 
        Int.compare h1 h2
    in
    let heapt = Pairing_heap.create ~min_size:100 ~cmp:compare_func () in

    let init_point = (i0, j0, NoDir, 1) in
    let min_heat = d_aux get_possible_nodes (i1, j1) heapt aa_ints path_map unvisited (init_point, 0) in
    min_heat

let dijkstra2 (i0, j0) (i1, j1) aa_ints = 
    let (dimi, dimj) = (Array.length aa_ints, Array.length aa_ints.(0)) in

    (* Generate the 4d arrays for the nodes *)
    let path_map = Bigarray.Genarray.create Bigarray.int Bigarray.c_layout [| dimi; dimj; 4; 10 |] in
    let unvisited = Bigarray.Genarray.create Bigarray.int Bigarray.c_layout [| dimi; dimj; 4; 10 |] in
    Bigarray.Genarray.fill path_map 100000;
    Bigarray.Genarray.fill unvisited 1;
           
    (* Initialize the priority queue where the ordering is based on the heat value *)
    let compare_func (x1, h1) (x2, h2) = 
        Int.compare h1 h2
    in
    let heapt = Pairing_heap.create ~min_size:100 ~cmp:compare_func () in

    let init_point = (i0, j0, NoDir, 1) in
    let min_heat = d_aux get_possible_nodes_uc (i1, j1) heapt aa_ints path_map unvisited (init_point, 0) in
    min_heat

let part1 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let list_list_ints = List.map ~f:(List.map ~f:Char.get_digit_exn) list_list_chars in
    let a_a_ints = List.to_array (List.map ~f:(List.to_array) list_list_ints) in
    dijkstra (0, 0) (140, 140) a_a_ints

let part2 filename = 
    let list_list_chars = Util.parse_to_list_list_chars filename in
    let list_list_ints = List.map ~f:(List.map ~f:Char.get_digit_exn) list_list_chars in
    let a_a_ints = List.to_array (List.map ~f:(List.to_array) list_list_ints) in
    dijkstra2 (0, 0) (140, 140) a_a_ints
