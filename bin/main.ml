open Core
open Aoc2023

let dump_to_screen p1 p2 t0 t1 t2 = 
    print_endline ("Part 1: " ^ (Int.to_string p1));
    print_endline ("Part 2: " ^ (Int.to_string p2));
    print_endline ("Time to run: " ^ (Int.to_string (t1-t0)) ^ " " ^ (Int.to_string (t2-t1)))

let problem17 filename = 
    print_endline "Problem 17:";
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p17p1 = Problem17.part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p17p2 = Problem17.part2 filename in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p17p1 p17p2 time0 time1 time2;
    ()

let problem16 filename = 
    print_endline "Problem 16:";
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p16p1 = Problem16.part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p16p2 = Problem16.part2 filename in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p16p1 p16p2 time0 time1 time2;
    ()

let problem15 filename = 
    print_endline "Problem 15:";
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p15p1 = Problem15.part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p15p2 = Problem15.part2 filename in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p15p1 p15p2 time0 time1 time2;
    ()

let problem13 filename = 
    print_endline "Problem 13:";
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p13p1 = Problem13.part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p13p2 = Problem13.part2 filename in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p13p1 p13p2 time0 time1 time2;
    ()
    
let problem12 filename = 
    print_endline "Problem 12:";
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p12p1 = Problem12.part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p12p2 = Problem12.part2 filename in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p12p1 p12p2 time0 time1 time2;
    ()

let problem11 filename = 
    print_endline "Problem 11:";
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p11p1 = Problem11.part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p11p2 = Problem11.part2 filename in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p11p1 p11p2 time0 time1 time2;
    ()

let problem10 filename = 
    print_endline "Problem 10:";
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p10p1 = Problem10.part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p10p2 = Problem10.part2_ver2 filename in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p10p1 p10p2 time0 time1 time2;
    ()
    
let problem9 filename = 
    print_endline "Problem 9:";
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p9p1 = Problem9.part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p9p2 = Problem9.part2 filename in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p9p1 p9p2 time0 time1 time2;
    ()

let problem7 filename = 
    print_endline "Problem 7:";
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p7p1 = Problem7.part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p7p2 = Problem7.part2 filename in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p7p1 p7p2 time0 time1 time2;
    ()

let problem6 filename = 
    print_endline "Problem 6:";
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p6p1 = Problem6.part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p6p2 = Problem6.part2 in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p6p1 p6p2 time0 time1 time2;
    ()

        
let problem5 filename = 
    print_endline "Problem 5:";
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p5p1 = Problem5.part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p5p2 = Problem5.part2 filename in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p5p1 p5p2 time0 time1 time2;
    ()

let problem4 filename = 
    print_endline "Problem 4:";
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p4p1 = Problem4.part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p4p2 = Problem4.part2 filename in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p4p1 p4p2 time0 time1 time2;
    ()

let problem3 filename = 
    print_endline "Problem 3:";
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p3p1 = Problem3.part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p3p2 = Problem3.part2 filename in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p3p1 p3p2 time0 time1 time2;
    ()

let problem2 filename = 
    print_endline "Problem 2:";
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p2p1 = Problem2.part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p2p2 = Problem2.part2 filename in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p2p1 p2p2 time0 time1 time2;
    ()

let problem1 filename = 
    print_endline "Problem 1:";
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p1p1 = Problem1.part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p1p2 = Problem1.part2 filename in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p1p1 p1p2 time0 time1 time2;
    ()

let () =
    let args = Sys.get_argv () in
    let filename = "inputs/input" ^ args.(1) ^ ".txt" in
    match Int.of_string args.(1) with
    | 1 -> problem1 "inputs/input1.txt"
    | 2 -> problem2 "inputs/input2.txt"
    | 3 -> problem3 "inputs/input3.txt"
    | 4 -> problem4 "inputs/input4.txt"
    | 5 -> problem5 "inputs/input5.txt"
    | 6 -> problem6 "inputs/input6.txt"
    | 7 -> problem7 "inputs/input7.txt"
    | 9 -> problem9 "inputs/input9.txt"
    | 10 -> problem10 "inputs/input10.txt"
    | 11 -> problem11 "inputs/input11.txt"
    | 12 -> problem12 filename
    | 13 -> problem13 filename
    | 15 -> problem15 filename
    | 16 -> problem16 filename
    | 17 -> problem17 "inputs/input17.txt"
    | _ -> print_string "Not implemented yet"
    
