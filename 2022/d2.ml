type game = Rock | Scissors | Paper

let read_lines file_name = let ic = open_in file_name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let char_to_game = function
  | 'A' | 'X' -> Rock
  | 'B' | 'Y' -> Paper
  | 'C' | 'Z' -> Scissors
  | _ -> failwith "Invalid char"

let process_input = List.map (fun x -> (char_to_game x.[0], char_to_game x.[2]))

let rec p1 lines = 
  let play_result = function
    | (aux1, aux2) when aux1 == aux2 -> 3
    | (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) -> 6
    | _ -> 0 in
  let play_points = function
    | (_, Rock) -> 1
    | (_, Paper) -> 2
    | (_, Scissors) -> 3 in
  let merged pair = play_result pair + play_points pair in
  List.map merged lines |> List.fold_left (+) 0

let p2 lines = 
  let win = function
    | Rock -> Paper
    | Paper -> Scissors
    | Scissors -> Rock in
  let play_points = function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3 in
  let merged = function
    | (first, Rock) -> play_points (win (win first))
    | (first, Paper) -> play_points first + 3
    | (first, Scissors) -> play_points (win first) + 6 in
  List.map merged lines |> List.fold_left (+) 0

let print_result = print_int

let () = read_lines "./inputs/i2.txt" |> process_input |> p2 |> print_result