open Core
open Aoc2025_lib

let () =
  let input = In_channel.read_all "input/day7.txt" in
  let output = Day7.Part2.run input in
  print_s [%message "" (output : int)]
;;
