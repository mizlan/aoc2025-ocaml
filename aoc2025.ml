open Core
open Aoc2025_lib

let () =
  let input = In_channel.read_all "input/day3.txt" in
  let output = Day3.Part2.run input in
  print_s [%message "" (output : int)]
