open! Core

module List = struct
  include List

  let drop_last_n lst ~n =
    let len = List.length lst in
    List.take lst (Int.max 0 (len - n))
  ;;
end

let parse s =
  let lines = String.split_lines s in
  List.map
    ~f:(fun bank_str ->
      bank_str
      |> String.to_list
      |> List.map ~f:(fun battery -> Char.to_int battery - Char.to_int '0'))
    lines
;;

module Part1 = struct
  let run s =
    let banks = parse s in
    List.sum
      (module Int)
      banks
      ~f:(fun bank ->
        let first_digit =
          bank
          |> List.drop_last_exn
          |> List.max_elt ~compare:[%compare: int]
          |> Option.value_exn
        in
        let second_digit =
          bank
          |> List.drop_while ~f:(fun x -> x <> first_digit)
          |> List.tl_exn
          |> List.max_elt ~compare:[%compare: int]
          |> Option.value_exn
        in
        (first_digit * 10) + second_digit)
  ;;
end

module Part2 = struct
  let rec get_max_joltage ~bank ~num_digits =
    if num_digits = 0
    then 0
    else (
      let candidates = List.drop_last_n ~n:(num_digits - 1) bank in
      let max_digit =
        candidates |> List.max_elt ~compare:[%compare: int] |> Option.value_exn
      in
      let remaining_list =
        bank |> List.drop_while ~f:(fun x -> x <> max_digit) |> List.tl_exn
      in
      (max_digit * Int.pow 10 (num_digits - 1))
      + get_max_joltage ~bank:remaining_list ~num_digits:(num_digits - 1))
  ;;

  let run s =
    let banks = parse s in
    List.sum
      (module Int)
      banks
      ~f:(fun bank ->
        let joltage = get_max_joltage ~bank ~num_digits:12 in
        print_s [%message "" (joltage : int)];
        joltage)
  ;;
end
