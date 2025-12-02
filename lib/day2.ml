open! Core

let parse s =
  let parse_range range_str =
    match
      String.split ~on:'-' range_str
      |> List.map ~f:String.strip |> List.map ~f:Int.of_string
    with
    | [ a; b ] -> (a, b)
    | _ -> failwith "range found with num components != 2"
  in
  let range_strings = String.split ~on:',' s in
  List.map ~f:parse_range range_strings

module Part1 = struct
  let id_is_invalid id =
    let s = Int.to_string id in
    if String.length s mod 2 <> 0 then false
    else
      let len = String.length s in
      let half = len / 2 in
      let x = String.subo ~len:half s in
      let y = String.subo ~pos:half s in
      String.(x = y)

  let run s =
    let ranges = parse s in
    let sum = ref 0 in
    List.iter ranges ~f:(fun (start, end_) ->
        for i = start to end_ do
          if id_is_invalid i then sum := !sum + i
        done);
    !sum
end

module Part2 = struct
  let repeat s n = String.concat ~sep:"" (List.init n ~f:(fun _ -> s))

  let id_is_invalid id =
    let s = Int.to_string id in
    let n = String.length s in
    let is_invalid = ref false in
    for width = 1 to n / 2 do
      let num_times = n / width in
      if num_times > 1 then
        let to_repeat = String.subo ~len:width s in
        let ideal = repeat to_repeat num_times in
        is_invalid := !is_invalid || String.(s = ideal)
    done;
    !is_invalid

  let run s =
    let ranges = parse s in
    let sum = ref 0 in
    List.iter ranges ~f:(fun (start, end_) ->
        for i = start to end_ do
          if id_is_invalid i then sum := !sum + i
        done);
    !sum
end
