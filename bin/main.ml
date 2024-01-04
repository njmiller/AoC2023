open Base
open Stdio
open Aoc2023

let dump_to_screen p1 p2 t0 t1 t2 = 
    Out_channel.output_string stdout "Part 1: ";
    Out_channel.output_string stdout (Int.to_string p1);
    Out_channel.output_string stdout "\n";
    Out_channel.output_string stdout "Part 2: ";
    Out_channel.output_string stdout (Int.to_string p2);
    Out_channel.output_string stdout "\n";
    Out_channel.output_string stdout "Time to run: ";
    Out_channel.output_string stdout (Int.to_string (t1-t0));
    Out_channel.output_string stdout " ";
    Out_channel.output_string stdout (Int.to_string (t2-t1));
    Out_channel.output_string stdout "\n";
    ()

let problem17 filename = 
    Out_channel.output_string stdout "Problem 17:\n";
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p17p1 = Problem17.part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p17p2 = Problem17.part2 filename in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p17p1 p17p2 time0 time1 time2;
    ()

let problem4 filename = 
    Out_channel.output_string stdout "Problem 4:\n";
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p4p1 = Problem4.part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p4p2 = Problem4.part2 filename in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p4p1 p4p2 time0 time1 time2;
    ()

let problem3 filename = 
    Out_channel.output_string stdout "Problem 3:\n";
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p3p1 = Problem3.part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p3p2 = Problem3.part2 filename in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p3p1 p3p2 time0 time1 time2;
    ()

let problem2 filename = 
    Out_channel.output_string stdout "Problem 2:\n";
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p2p1 = Problem2.part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p2p2 = Problem2.part2 filename in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p2p1 p2p2 time0 time1 time2;
    ()

let problem1 filename = 
    Out_channel.output_string stdout "Problem 1:\n";
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p1p1 = Problem1.part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p1p2 = Problem1.part2 filename in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p1p1 p1p2 time0 time1 time2;
    ()

let () = 
    problem4 "inputs/input4.txt"
