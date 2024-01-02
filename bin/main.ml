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

let problem2 filename = 
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    Out_channel.output_string stdout "Problem 2:\n";
    let p2p1 = Problem2.part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p2p2 = Problem2.part2 filename in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p2p1 p2p2 time0 time1 time2;
    ()

let problem1 filename = 
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    Out_channel.output_string stdout "Problem 1:\n";
    let p1p1 = Problem1.part1 filename in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p1p2 = Problem1.part2 filename in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    dump_to_screen p1p1 p1p2 time0 time1 time2;
    ()

let () = 
    problem1 "inputs/input1.txt"
