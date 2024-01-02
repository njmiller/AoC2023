open Base
open Stdio
open Aoc2023

let () =
    let time0 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    Out_channel.output_string stdout "Problem 1:\n";
    let p1p1 = Problem1.part1 "inputs/input1.txt" in
    let time1 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    let p1p2 = Problem1.part2 "inputs/input1.txt" in
    let time2 = Int63.to_int_exn (Time_now.nanoseconds_since_unix_epoch ()) in
    Out_channel.output_string stdout "Part 1: ";
    Out_channel.output_string stdout (Int.to_string p1p1);
    Out_channel.output_string stdout "\n";
    Out_channel.output_string stdout "Part 2: ";
    Out_channel.output_string stdout (Int.to_string p1p2);
    Out_channel.output_string stdout "\n";
    Out_channel.output_string stdout "Time to run: ";
    Out_channel.output_string stdout (Int.to_string (time1-time0));
    Out_channel.output_string stdout " ";
    Out_channel.output_string stdout (Int.to_string (time2-time1));
    Out_channel.output_string stdout "\n";
