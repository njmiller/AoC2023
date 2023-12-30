open Base
open Stdio
open Aoc2023

let () = 
    Out_channel.output_string stdout "Problem 1:\n";
    let p1p1 = Problem1.part1 "inputs/input1.txt" in
    let p1p2 = Problem1.part2 "inputs/input1.txt" in
    Out_channel.output_string stdout "Part 1: ";
    Out_channel.output_string stdout (Int.to_string p1p1);
    Out_channel.output_string stdout "\n";
    Out_channel.output_string stdout "Part 2: ";
    Out_channel.output_string stdout (Int.to_string p1p2);
    Out_channel.output_string stdout "\n";
