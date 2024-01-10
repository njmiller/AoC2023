open Core

type hstone = {x: float; y: float; z: float; vx: float; vy: float; vz: float}

let line_to_hstone line = 
    (*
    let str_nums = String.split_on_chars line ~on:[','; '@'] in
    let str_nums = List.map ~f:String.strip str_nums in
    let nums = List.map ~f:Int.of_string str_nums
    *)

    let nums = String.split_on_chars line ~on:[','; '@'] 
        |> List.map ~f:String.strip
        |> List.map ~f:Float.of_string
    in

    let [x; y; z; vx; vy; vz] = nums in
    {x=x; y=y; z=z; vx=vx; vy=vy; vz=vz}

let calc_intersection hstone1 hstone2 = 
    let dx = hstone1.x -. hstone2.x in
    let dy = hstone1.y -. hstone2.y in
    let ytmp = dy /. hstone2.vy in
    let xtmp = dx /. hstone2.vx in
    let dvxrat = hstone1.vx /. hstone2.vx in
    let dvyrat = hstone1.vy /. hstone2.vy in

    let a = dvyrat -. dvxrat in
    let t1 = (xtmp -. ytmp) /. a in
    let t2 = xtmp +. dvxrat *. t1 in
    let xint = hstone1.x +. hstone1.vx *. t1 in
    let yint = hstone1.y +. hstone1.vy *. t1 in
    let xint2 = hstone2.x +. hstone2.vx *. t2 in
    let yint2 = hstone2.y +. hstone2.vy *. t2 in

    if Float.compare t1 0.0 = 1 && Float.compare t2 0.0 = 1 then
        Some (xint, yint)
    else
        None
    (*(t1, t2, xint, yint, xint2, yint2)*)

let calc_intersection2 hstone1 hstone2 = 
    let dx = hstone1.x -. hstone2.x in
    let dy = hstone1.y -. hstone2.y in
    let ytmp = dy /. hstone2.vy in
    let xtmp = dx /. hstone2.vx in
    let dvxrat = hstone1.vx /. hstone2.vx in
    let dvyrat = hstone1.vy /. hstone2.vy in

    let a = dvyrat -. dvxrat in
    let t1 = (xtmp -. ytmp) /. a in
    let t2 = xtmp +. dvxrat *. t1 in
    let xint = hstone1.x +. hstone1.vx *. t1 in
    let yint = hstone1.y +. hstone1.vy *. t1 in
    let xint2 = hstone2.x +. hstone2.vx *. t2 in
    let yint2 = hstone2.y +. hstone2.vy *. t2 in

    if Float.compare t1 0.0 = 1 && Float.compare t2 0.0 = 1 then
        Some (t1, t2, xint, yint)
    else
        None

let rec calc_all_intersections list_hstones = 
    match list_hstones with
    | [] -> []
    | h :: t ->
            let inters = List.map ~f:(calc_intersection h) t in
            inters @ calc_all_intersections t

let int_is_in_area (xmin, xmax) (ymin, ymax) intersection = 
    match intersection with
    | Some (x, y) -> Float.compare x xmin >= 0 && Float.compare x xmax <= 0 && Float.compare y ymin >= 0 && Float.compare y ymax <= 0
    | None -> false

let rec get_subset list_hstones nsubset = 
    match nsubset with
    | 0 -> []
    | _ ->
            let h :: t = list_hstones in
            h :: get_subset t (nsubset-1)

let rec check_same_inters inter1 inter2 = 
    match inter1 with
    | None -> false
    | Some (t1, t2, xint1, yint1) ->
            let xint1 = Float.round xint1 in
            let yint1 = Float.round yint1 in
            (*let xint1b = Int.of_float_unchecked xint1 in
            let yint1b = Int.of_float_unchecked yint1 in*)
            (match inter2 with
             | None -> false
             | Some (t1, t2, xint2, yint2) ->
                     let xint2 = Float.round xint2 in
                     let yint2 = Float.round yint2 in
                     (*let xint2b = Int.of_float_unchecked xint2 in
                     let yint2b = Int.of_float_unchecked yint2 in
                     xint1b = xint2b && yint1b = yint2b*)
                     Float.equal xint1 xint2 && Float.equal yint1 yint2
            )

let get_ts inters = 
    let map_func inter = 
        match inter with
        | Some (t1, t2, xint, yint) -> t2
        | None -> -1000.0
    in

    let ts = List.map ~f:map_func inters in
    let Some (t1, _, _, _) = List.hd_exn inters in
    t1 :: ts

let solve_vz (hstone1, t1) (hstone2, t2) = 
    let dz = hstone1.z -. hstone2.z in
    let dz2 = hstone1.vz*.t1 -. hstone2.vz*.t2 in
    let dt = t1 -. t2 in
    (dz +. dz2) /. dt

let rec solve_aux list_hstones xyvels =
    (*let (xvel, yvel) = List.hd_exn xyvels in*)
    let (xvel, yvel) :: _ = xyvels in
    let (xvel, yvel) = (Float.of_int xvel, Float.of_int yvel) in
    let list_new = List.map ~f:(fun hs -> {hs with vx=hs.vx-.xvel; vy=hs.vy-.yvel}) list_hstones in
    let h :: t = list_new in
    let inters = List.map ~f:(calc_intersection2 h) t in
    let i0 :: irest = inters in
    let all_same = List.map ~f:(check_same_inters i0) irest in

    (* If all the new stones intersect at the same point, solve for z-velocity *)
    if List.reduce_exn ~f:(&&) all_same then
        let get_xyint (Some (a, b, c, d)) = (c, d) in
        let (xint, yint) = get_xyint i0 in
        let hstonest = get_ts inters |> List.zip_exn list_hstones in
        let h0 :: h1 :: _ = hstonest in
        let vz = solve_vz h0 h1 in
        let (hstone, time) = h0 in
        let zint = hstone.z +. (hstone.vz -. vz)*.time in
        (xint, yint, zint)
    else
        let xyvels_new = List.tl_exn xyvels in
        solve_aux list_hstones xyvels_new


let solve_for_pos_vel list_hstones = 
    let xvel = Util.range (-500) 501 in
    let yvel = Util.range (-500) 501 in

    let xyvel = List.cartesian_product xvel yvel in
    solve_aux list_hstones xyvel

let part1 filename = 
    let list_lines = Util.parse_to_lines filename in
    let list_hstones = List.map ~f:line_to_hstone list_lines in
    let intersections = calc_all_intersections list_hstones in
    let (xymin, xymax) =  (200000000000000.0, 400000000000000.0) in
    (*let is_in_area = List.filter ~f:(int_is_in_area (7.0, 27.0) (7.0, 27.0)) intersections in*)
    let is_in_area = List.filter ~f:(int_is_in_area (xymin, xymax) (xymin, xymax)) intersections in
    List.length is_in_area

let part2 filename = 
    let list_lines = Util.parse_to_lines filename in
    let list_hstones = List.map ~f:line_to_hstone list_lines in
    let list_hstones = get_subset list_hstones 5 in
    let (xpos, ypos, zpos) = solve_for_pos_vel list_hstones in
    Int.of_float (xpos +. ypos +. zpos)
    (*solve_aux list_hstones [(-10, 5); (-3, 1)]*)
