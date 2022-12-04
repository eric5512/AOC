let read_lines file_name = let ic = open_in file_name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let process_input lines = ()

let p1 = ()

let p2 = ()

let print_result result = ()

let () = read_lines "./inputs/i.txt" |> process_input |> print_result