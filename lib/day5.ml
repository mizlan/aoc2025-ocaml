open! Core

let parse s =
  let lines = String.split_lines s in
  let fresh_range_strs, remaining_lines =
    List.split_while ~f:(fun x -> not (String.is_empty x)) lines
  in
  let fresh_ranges = List.map ~f:Day2.parse_range fresh_range_strs in
  let available_ingredients =
    remaining_lines |> List.tl_exn
    |> List.take_while ~f:(fun x -> not (String.is_empty x))
    |> List.map ~f:Int.of_string
  in
  (fresh_ranges, available_ingredients)

module Part1 = struct
  let run s =
    let fresh_ranges, available_ingredients = parse s in
    let ingredient_is_fresh ingr =
      List.exists fresh_ranges ~f:(fun (start, end_) ->
          start <= ingr && ingr <= end_)
    in
    available_ingredients |> List.filter ~f:ingredient_is_fresh |> List.length
end

module Part2 = struct
  let run s =
    let fresh_ranges, _ = parse s in
    let fresh_ranges = List.sort fresh_ranges ~compare:[%compare: int * int] in
    let rec merge_intervals = function
      | ([] | [ _ ]) as xs -> xs
      | (a, b) :: (c, d) :: xs when b >= c ->
          merge_intervals ((min a c, max b d) :: xs)
      | x :: xs -> x :: merge_intervals xs
    in
    let merged_ranges = merge_intervals fresh_ranges in
    List.sum
      (module Int)
      merged_ranges
      ~f:(fun (start, end_) -> end_ - start + 1)
end
