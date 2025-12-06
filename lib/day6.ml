open! Core

module String = struct
  include String

  let split_on_whitespace s = Re2.split (Re2.create_exn "\\s+") s
end

module List = struct
  include List

  let fold1_exn xs ~f =
    match xs with
    | [] -> failwith "Cannot call fold1_exn on an empty list"
    | x :: xs -> List.fold xs ~init:x ~f
  ;;

  (* This is ugly *)
  let group_without ~f xs =
    let grouped = List.group xs ~break:(fun a b -> f a || f b) in
    List.filter grouped ~f:(function
      | [ x ] when f x -> false
      | _ -> true)
  ;;
end

let get_operators line =
  let parse_operation = function
    | "*" -> ( * )
    | "+" -> ( + )
    | op -> raise_s [%message "unexpected operator" op]
  in
  line |> String.strip |> String.split_on_whitespace |> List.map ~f:parse_operation
;;

let solve_problems problems =
  let solve_problem (op, operands) = List.fold1_exn ~f:op operands in
  List.sum (module Int) ~f:solve_problem problems
;;

module Part1 = struct
  let parse s =
    let lines = String.split_lines s in
    let operators = lines |> List.last_exn |> get_operators in
    let operands =
      lines
      |> List.drop_last_exn
      |> List.map ~f:String.strip
      |> List.map ~f:String.split_on_whitespace
      |> List.transpose_exn
      |> List.map ~f:(List.map ~f:Int.of_string)
    in
    List.zip_exn operators operands
  ;;

  let run s =
    let problems = parse s in
    solve_problems problems
  ;;
end

module Part2 = struct
  let parse s =
    let lines = String.split_lines s in
    let operators = lines |> List.last_exn |> get_operators in
    let operands =
      lines
      |> List.drop_last_exn
      |> List.map ~f:String.to_list
      |> List.transpose_exn
      |> List.map ~f:String.of_char_list
      |> List.map ~f:String.strip
      |> List.group_without ~f:String.is_empty
      |> List.map ~f:(List.map ~f:Int.of_string)
    in
    List.zip_exn operators operands
  ;;

  let run s =
    let problems = parse s in
    solve_problems problems
  ;;
end
