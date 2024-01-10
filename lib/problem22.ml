open Core

type block = {r0: int*int*int; r1: int*int*int; orientation: char; size: int; id: int; supported_by: int list; supports: int list}

let compare_x1 block1 block2 = 
    let (x1, y1, z1) = block1.r1 in
    let (x2, y2, z2) = block2.r1 in
    Int.compare x1 x2

let compare_y1 block1 block2 = 
    let (x1, y1, z1) = block1.r1 in
    let (x2, y2, z2) = block2.r1 in
    Int.compare y1 y2

let compare_z0 block1 block2 = 
    let (x1, y1, z1) = block1.r0 in
    let (x2, y2, z2) = block2.r0 in
    Int.compare z1 z2

let compare_z1 block1 block2 = 
    let (x1, y1, z1) = block1.r1 in
    let (x2, y2, z2) = block2.r1 in
    Int.compare z1 z2

let parse_blocks i line = 
    let nums = String.split_on_chars ~on:[','; '~'] line in
    let [x0; y0; z0; x1; y1; z1] = List.map ~f:Int.of_string nums in
    let (size, orientation) = if (x1-x0) > 0 then
                                  (x1-x0+1, 'X')
                              else if (y1-y0) > 0 then
                                  (y1-y0+1, 'Y')
                              else
                                  (z1-z0+1, 'Z')
    in
    {r0=(x0, y0, z0); r1=(x1, y1, z1); orientation=orientation; size=size;
     id=i; supported_by=[]; supports=[]}

let propagate_one_down settled zheights block = 
  
    (* Extract the location of the block from the record *)
    let (x0, y0, z0) = block.r0 in
    let (x1, y1, z1) = block.r1 in

    (* Get a list of the Z-height and ID at that height for each X,Y location that tthis current block takes up *)
    let heights = 
        (match block.orientation with
         | 'X' -> 
                let xrange = Util.range x0 (x1+1) in
                List.map ~f:(fun x -> zheights.(x).(y0)) xrange
         | 'Y' ->
                let yrange = Util.range y0 (y1+1) in
                List.map ~f:(fun y -> zheights.(x0).(y)) yrange
         | 'Z' -> [zheights.(x0).(y0)]
        ) in

    (* Get the value of the max height along the X,Y locations of the block *)
    let compare_aux (a0, a1) (b0, b1) = 
        Int.compare a0 b0
    in
    let maxval = List.max_elt ~compare:compare_aux heights in
    let maxheight = (match maxval with
                     | Some (x, y) -> x
                     | None -> 0
                    )
    in
    
    (* Get all the elements at this height *)
    let supports = List.filter ~f:(fun (x, y) -> x = maxheight) heights in

    (* Remove duplicate IDs and get the actual height *)
    let (height, ids) = List.unzip supports in
    let height = Util.extract (List.hd height) in
    let ids = List.remove_consecutive_duplicates ~equal:Int.equal ids in
    let ids = List.filter ~f:(fun x -> x >= 0) ids in

    (* Update the minz array *)
    let _ = (match block.orientation with
             | 'X' ->
                     let xrange = Util.range x0 (x1+1) in
                     let _ = List.map ~f:(fun x -> zheights.(x).(y0) <- (height+1, block.id)) xrange in
                     ()
             | 'Y' ->
                     let yrange = Util.range y0 (y1+1) in
                     let _ = List.map ~f:(fun y -> zheights.(x0).(y) <- (height+1, block.id)) yrange in
                     ()
             | 'Z' -> 
                     let _ = zheights.(x0).(y0) <- (height+block.size, block.id) in
                     () 
            ) 
    in
    
    (* Update the supports array for each id in IDs. They should already be in the settles array *)
    let update_supports id = 
        let block_old = settled.(id) in
        let block_new = {block_old with supports=(block.id :: block_old.supports)} in
        settled.(id) <- block_new
    in
    let _ = List.map ~f:update_supports ids in
    
    (* Generate the new block to store in the settled array *)
    let r0_new = (x0, y0, height+1) in
    let r1_new = if Char.equal block.orientation 'Z' then
                     (x1, y1, height+block.size)
                 else 
                     (x1, y1, height+1)
    in
    let block_new = {block with r0=r0_new; r1=r1_new; supported_by=ids} in
    settled.(block.id) <- block_new;
    settled

let rec propagate_all_down zheight settled unsettled = 
    match unsettled with
    | [] -> settled
    | h :: t -> 
            let settled = propagate_one_down settled zheight h in
            propagate_all_down zheight settled t

let get_dims list_blocks = 
    let maxx_elt = List.max_elt ~compare:compare_x1 list_blocks in
    let maxy_elt = List.max_elt ~compare:compare_y1 list_blocks in

    let maxx = (match maxx_elt with
                | Some res -> let (tmp, _, _) = res.r1 in tmp
                | None -> -1
               ) in
    
    let maxy = (match maxy_elt with
                | Some res -> let (_, tmp, _) = res.r1 in tmp
                | None -> -1
               ) in
    (maxx+1, maxy+1)

let find_disentigrate settled =
    let check_can_disentigrate block = 
        match List.length(block.supports) with
        | 0 -> true
        | _ ->
                let has_mult_supports = List.map ~f:(fun x -> List.length settled.(x).supported_by > 1) block.supports in
                List.reduce_exn ~f:(&&) has_mult_supports
    in

    let can_disentigrate = Array.map ~f:check_can_disentigrate settled in
    Array.length (Array.filter ~f:(fun x -> x) can_disentigrate)

let rec find_falling is_falling settled to_check =
    let check_is_falling id = 
        let supported_by = settled.(id).supported_by in
        let supports_falling = List.map ~f:(fun x -> is_falling.(x)) supported_by in
        let is_falling_opt = List.reduce ~f:(&&) supports_falling in
        match is_falling_opt with
        | Some res -> res
        | None -> false (* this is true if it is not supported by anything *)
    in
    match to_check with
    | [] -> is_falling
    | h :: t ->
            let is_id_falling = check_is_falling h in
            (* If it is already marked as falling we alreadu check what it supposed*)
            let to_check2 = if is_id_falling && (not is_falling.(h)) then
                               t @ settled.(h).supports
                           else
                               t
            in
            let _ = is_falling.(h) <- is_id_falling in
            find_falling is_falling settled to_check2
    
let calc_all_falling settled id = 
    let nblocks = Array.length settled in
    let is_falling = Array.create ~len:nblocks false in
    is_falling.(id) <- true;
    find_falling is_falling settled settled.(id).supports

let calc_chain_reaction settled =
    let nblocks = Array.length settled in
    let falling = List.map ~f:(calc_all_falling settled) (Util.range 0 nblocks) in

    let map_func fall_bool = Array.filter ~f:(fun x -> x) fall_bool in
    let nums = List.map ~f:(fun x -> Array.length (map_func x)) falling in
    (List.reduce_exn ~f:(+) nums) - nblocks

let part1 filename = 
    let list_lines = Util.parse_to_lines filename in
    let list_blocks = List.mapi ~f:parse_blocks list_lines in

    (* MinZ contains a list of the height at each X,Y value where a block will first encounter another block and the id of those blocks *)
    let (maxx, maxy) = get_dims list_blocks in
    let minz = Array.make_matrix ~dimx:maxx ~dimy:maxy (0, -1) in

    (* Sort by the z0 value so that when we move each block downward we don't hit another block that hasn't yet settled *)
    let list_blocks = List.sort ~compare:compare_z0 list_blocks in

    let block0 = {r0=(0, 0, 0); r1=(0, 0, 0); size=1; orientation='X'; id=(-1);
                  supports=[]; supported_by=[]} in
    let settled = Array.create ~len:(List.length list_blocks) block0 in
    let settled = propagate_all_down minz settled list_blocks in
    find_disentigrate settled
    
let part2 filename = 
    let list_lines = Util.parse_to_lines filename in
    let list_blocks = List.mapi ~f:parse_blocks list_lines in

    (* MinZ contains a list of the height at each X,Y value where a block will first encounter another block and the id of those blocks *)
    let (maxx, maxy) = get_dims list_blocks in
    let minz = Array.make_matrix ~dimx:maxx ~dimy:maxy (0, -1) in

    (* Sort by the z0 value so that when we move each block downward we don't hit another block that hasn't yet settled *)
    let list_blocks = List.sort ~compare:compare_z0 list_blocks in

    let block0 = {r0=(0, 0, 0); r1=(0, 0, 0); size=1; orientation='X'; id=(-1);
                  supports=[]; supported_by=[]} in
    let settled = Array.create ~len:(List.length list_blocks) block0 in
    let settled = propagate_all_down minz settled list_blocks in
    calc_chain_reaction settled
