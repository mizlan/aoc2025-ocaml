open Core
open Aoc2025_lib

let () =
  let input = In_channel.read_all "input/day1.txt" in
  let output = Day1.Part2.run input in
  print_s [%message "" (output : int)]
