open! Core

let parse s =
  match String.split_lines s with
  | [] -> failwith "must have at least one line"
  | start :: rest ->
    let start_position = String.index_exn start 'S' in
    let find_all_splitters line =
      line
      |> String.to_list
      |> List.filter_mapi ~f:(fun i -> function
        | '^' -> Some i
        | _ -> None)
      |> Int.Set.of_list
    in
    let splitter_positions = List.map rest ~f:find_all_splitters in
    start_position, splitter_positions
;;

module Part1 = struct
  let run s =
    let start_position, splitter_positions = parse s in
    let _, count =
      List.fold
        splitter_positions
        ~init:(Int.Set.singleton start_position, 0)
        ~f:(fun (beams, count) splitters ->
          let collide_set = Set.inter beams splitters in
          let newly_generated_set =
            let leftward_splits = Int.Set.map collide_set ~f:(fun x -> x - 1) in
            let rightward_splits = Int.Set.map collide_set ~f:(fun x -> x + 1) in
            Set.union leftward_splits rightward_splits
          in
          let beams' = Set.diff beams collide_set |> Set.union newly_generated_set in
          beams', count + Set.length collide_set)
    in
    count
  ;;
end

module Part2 = struct
  let run s =
    let start_position, splitter_positions = parse s in
    let count_ways_exiting_manifold_by_position =
      List.fold
        splitter_positions
        ~init:(Int.Map.singleton start_position 1)
        ~f:(fun beam_ways splitters ->
          let collided, idle =
            Map.partitioni_tf beam_ways ~f:(fun ~key:pos ~data:_ -> Set.mem splitters pos)
          in
          let merge_with_add a b =
            Map.merge_skewed a b ~combine:(fun ~key:_ a b -> a + b)
          in
          let newly_generated =
            let leftward_splits = Int.Map.map_keys_exn collided ~f:(fun x -> x - 1) in
            let rightward_splits = Int.Map.map_keys_exn collided ~f:(fun x -> x + 1) in
            merge_with_add leftward_splits rightward_splits
          in
          merge_with_add idle newly_generated)
    in
    Map.sum (module Int) count_ways_exiting_manifold_by_position ~f:Fun.id
  ;;
end
