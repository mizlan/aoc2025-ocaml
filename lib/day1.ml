open! Core

let parse s =
  let parse_line line =
    let dir =
      match line.[0] with
      | 'L' -> -1
      | 'R' -> 1
      | _ -> failwith "bad"
    in
    let amount = Int.of_string (String.subo ~pos:1 line) in
    dir * amount
  in
  let lines = String.split_lines s in
  List.map ~f:parse_line lines
;;

module Part1 = struct
  let run s =
    let rotations = parse s in
    let rotate ~phase ~rotation =
      let phase' = phase + rotation in
      phase' % 100
    in
    let acc, _ =
      List.fold_left
        rotations
        ~init:(0 (* acc *), 50 (* phase *))
        ~f:(fun (acc, phase) rotation ->
          let phase = rotate ~phase ~rotation in
          let incr = if phase = 0 then 1 else 0 in
          let acc = acc + incr in
          print_s [%message "" (acc : int) (phase : int)];
          acc, phase)
    in
    acc
  ;;
end

module Part2 = struct
  let part2 rotations =
    let num_zeros, _ =
      List.fold_left rotations ~init:(0, 50) ~f:(fun (acc, phase) rotation ->
        print_s [%message "rotating from" (phase : int) (rotation : int)];
        let short_zero, small_rotation =
          match () with
          | () when phase = 0 ->
            print_s [%message "skipping small since phase = 0"];
            0, 0
          | () when rotation <= 0 && phase > 0 && phase + rotation <= 0 ->
            print_s [%message "underflow rotation"];
            1, -phase
          | () when rotation >= 0 && phase + rotation >= 100 ->
            print_s [%message "overflow rotation"];
            1, 100 - phase
          | () ->
            print_s [%message "none"];
            0, rotation
        in
        let additional_zeros =
          let remaining_rotation = rotation - small_rotation in
          abs remaining_rotation / 100
        in
        if additional_zeros > 0 then print_s [%message "adding" (additional_zeros : int)];
        let phase' = (phase + rotation) % 100 in
        acc + short_zero + additional_zeros, phase')
    in
    num_zeros
  ;;

  module Slow = struct
    let part2 rotations =
      let phase = ref 50 in
      let num_zeros = ref 0 in
      List.iter rotations ~f:(fun rotation ->
        match rotation with
        | 0 -> ()
        | _ ->
          let single = rotation / abs rotation in
          for _ = 1 to abs rotation do
            phase := (!phase + single) % 100;
            if !phase = 0 then incr num_zeros
          done);
      !num_zeros
    ;;
  end

  let run s =
    let rotations = parse s in
    part2 rotations
  ;;

  let%expect_test "underflow" =
    let num_zeros = part2 [ -60 ] in
    print_s [%message "" (num_zeros : int)];
    [%expect
      " \n\
      \ (\"rotating from\" (phase 50) (rotation -60))\n\
      \ \"underflow rotation\"\n\
      \ (num_zeros 1)\n\
      \ "]
  ;;

  let%expect_test "on dot" =
    let num_zeros = part2 [ -50; 1; -1 ] in
    print_s [%message "" (num_zeros : int)];
    [%expect
      " \n\
      \ (\"rotating from\" (phase 50) (rotation -50))\n\
      \ \"underflow rotation\"\n\
      \ (\"rotating from\" (phase 0) (rotation 1))\n\
      \ none\n\
      \ (\"rotating from\" (phase 1) (rotation -1))\n\
      \ \"underflow rotation\"\n\
      \ (num_zeros 2)\n\
      \ "]
  ;;

  let%expect_test "never on dot" =
    let num_zeros = part2 [ -60; 60; -60; 60 ] in
    print_s [%message "" (num_zeros : int)];
    [%expect
      " \n\
      \ (\"rotating from\" (phase 50) (rotation -60))\n\
      \ \"underflow rotation\"\n\
      \ (\"rotating from\" (phase 90) (rotation 60))\n\
      \ \"overflow rotation\"\n\
      \ (\"rotating from\" (phase 50) (rotation -60))\n\
      \ \"underflow rotation\"\n\
      \ (\"rotating from\" (phase 90) (rotation 60))\n\
      \ \"overflow rotation\"\n\
      \ (num_zeros 4)\n\
      \ "]
  ;;

  let%expect_test "on dot multi-revolution" =
    let num_zeros = part2 [ -50; 200 ] in
    print_s [%message "" (num_zeros : int)];
    [%expect
      " \n\
      \ (\"rotating from\" (phase 50) (rotation -50))\n\
      \ \"underflow rotation\"\n\
      \ (\"rotating from\" (phase 0) (rotation 200))\n\
      \ \"overflow rotation\"\n\
      \ (adding (additional_zeros 1))\n\
      \ (num_zeros 3)\n\
      \ "]
  ;;

  let%expect_test "slow never on dot" =
    let num_zeros = Slow.part2 [ -50; 200 ] in
    print_s [%message "" (num_zeros : int)];
    [%expect "(num_zeros 3)"]
  ;;

  let smallish_list_gen = List.gen_with_length 10 (Int.gen_incl (-100) 100)

  let%expect_test "individual" =
    let num_zeros = Slow.part2 [ 47; 47; 50; 73; -54; 87; -100 ] in
    print_s [%message "" (num_zeros : int)];
    [%expect]
  ;;

  let%expect_test "fast" =
    let num_zeros = part2 [ 47; 47; 50; 73; -54; 87; -100 ] in
    print_s [%message "" (num_zeros : int)];
    [%expect]
  ;;

  let%quick_test "slow is same as fast" =
    fun (rotations : (int list[@generator smallish_list_gen])) ->
    assert (Slow.part2 rotations = part2 rotations);
    [%expect]
  ;;
end
