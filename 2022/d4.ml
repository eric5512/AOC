let read_lines file_name = let ic = open_in file_name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let process_input =
  let parse str = Scanf.sscanf str "%d-%d,%d-%d" (fun x y z w -> ((x, y), (z, w))) in
  List.map parse

let p1 lines = 
  let contains ((l1, h1), (l2, h2)) = l1 >= l2 && h1 <= h2 in
  List.map (fun (r1,r2) -> if contains (r2,r1) || contains (r1,r2) then 1 else 0) lines |> List.fold_left (+) 0

let p2 lines =
  let overlaps ((_, high), (low, _)) = if high >= low then 1 else 0 in
  List.map (fun (r1,r2) -> if fst r2 > fst r1 then overlaps (r1,r2) else overlaps (r2,r1)) lines |> List.fold_left (+) 0

let print_result = print_int

let () = read_lines "./inputs/i4.txt" |> process_input |> p2 |> print_result