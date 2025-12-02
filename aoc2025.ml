open Core
open Aoc2025_lib

let () =
  let input = In_channel.read_all "input/day2.txt" in
  let output = Day2.Part2.run input in
  print_s [%message "" (output : int)]
