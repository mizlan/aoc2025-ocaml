open! Core

let parse s =
  let lines = s |> String.split_lines |> List.to_array in
  Array.map lines ~f:String.to_array

module Part1 = struct
  let accessible grid =
    let r = Array.length grid in
    let c = Array.length grid.(0) in
    let accessible i j =
      let deltas =
        List.cartesian_product [ 0; -1; 1 ] [ 0; -1; 1 ] |> List.tl_exn
      in
      let surrounding_rolls =
        List.filter deltas ~f:(fun (di, dj) ->
            let i' = i + di in
            let j' = j + dj in
            0 <= i' && i' < r && 0 <= j' && j' < c
            && Char.(grid.(i').(j') = '@'))
      in
      List.length surrounding_rolls < 4
    in
    let accessible_positions =
      let rows = List.init r ~f:(fun x -> x) in
      let cols = List.init c ~f:(fun x -> x) in
      List.filter (List.cartesian_product rows cols) ~f:(fun (i, j) ->
          Char.(grid.(i).(j) = '@') && accessible i j)
    in
    accessible_positions

  let run s =
    let grid = parse s in
    List.length (accessible grid)
end

module Part2 = struct
  let run s =
    let grid = parse s in

    let rec repeatedly_remove_accessible grid count =
      let accessible = Part1.accessible grid in
      if List.is_empty accessible then count
      else (
        List.iter accessible ~f:(fun (i, j) -> grid.(i).(j) <- '.');
        repeatedly_remove_accessible grid (count + List.length accessible))
    in
    repeatedly_remove_accessible grid 0
end
