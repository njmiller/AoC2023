open Core
open Aoc2023

let dump_to_screen p1 p2 t0 t1 t2 = 
    print_endline ("Part 1: " ^ (Int.to_string p1));
    print_endline ("Part 2: " ^ (Int.to_string p2));
    print_endline ("Time to run: " ^ (Int.to_string (t1-t0)) ^ " " ^ (Int.to_string (t2-t1)))

let part1s = [| Problem1.part1; Problem2.part1; Problem3.part1; Problem4.part1; Problem5.part1;
                Problem6.part1; Problem7.part1; Problem8.part1; Problem9.part1; Problem10.part1;
                Problem11.part1; Problem12.part1; Problem13.part1; Problem14.part1; Problem15.part1;
                Problem16.part1; Problem17.part1; Problem18.part1; Problem19.part1; Problem20.part1;
                Problem21.part1; Problem22.part1; Problem23.part1; Problem24.part1; Problem25.part1; |]

let part2s = [| Problem1.part2; Problem2.part2; Problem3.part2; Problem4.part2; Problem5.part2;
                Problem6.part2; Problem7.part2; Problem8.part2; Problem9.part2; Problem10.part2;
                Problem11.part2; Problem12.part2; Problem13.part2; Problem14.part2; Problem15.part2;
                Problem16.part2; Problem17.part2; Problem18.part2; Problem19.part2; Problem20.part2;
                Problem21.part2; Problem22.part2; Problem23.part2; Problem24.part2; Problem25.part2; |]

let run_problem part1 part2 num =
    let filename = "inputs/input" ^ Int.to_string num ^ ".txt" in 
    print_endline ("Problem " ^ Int.to_string num ^ ":");
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p1 = part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p2 = part2 filename in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p1 p2 time0 time1 time2;
    ()

let () =
    let args = Sys.get_argv () in
    let arg_int = Int.of_string args.(1) in
    let part1 = part1s.(arg_int-1) in
    let part2 = part2s.(arg_int-1) in
    run_problem part1 part2 arg_int
    
