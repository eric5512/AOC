let read_lines file_name = let ic = open_in file_name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let process_input = 
  let rec process_aux acc = function
    | hd::""::tl -> (acc + int_of_string(hd))::(process_aux 0 tl)
    | hd::tl -> process_aux (acc + int_of_string(hd)) tl
    | [] -> [acc] in
  process_aux 0

let p1 = List.fold_left max 0

let p2 = 
  let rec p2_aux acc1 acc2 acc3 = function
    | hd::tl -> if hd > acc1 then
      p2_aux hd acc1 acc2 tl
    else if hd > acc2 then
      p2_aux acc1 hd acc2 tl
    else if hd > acc3 then
      p2_aux acc1 acc2 hd tl
    else
      p2_aux acc1 acc2 acc3 tl
    | [] -> acc1 + acc2 + acc3 in
  p2_aux 0 0 0

let print_result = print_int

let () = read_lines "./inputs/i1.txt" |> process_input |> p2 |> print_result