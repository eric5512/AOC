let read_lines file_name = let ic = open_in file_name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let process_input = 
  let aux_func str = 
    let size = (String.length str)/2 in
  (String.sub str 0 size, String.sub str size size) in
  List.map aux_func

let rec process_input2 = function
  | hd0::hd1::hd2::tl -> (hd0, hd1, hd2)::process_input2 tl
  | [] -> []
  | _ -> failwith "Invalid number of lines"

let priority c = let code = Char.code c in if c >= 'a' && c <= 'z' then code - Char.code 'a' + 1 else if c >= 'A' && c <= 'Z' then code - Char.code 'A' + 27 else failwith "Invalid character"

let p1 lines = 
  let find_prior (s1, s2) = let size = String.length s1 in let aux_char = ref '-' in
    for i = 0 to size - 1 do
      if String.contains s2 s1.[i] then aux_char := s1.[i];
    done;
    priority !aux_char in
  List.map find_prior lines |> List.fold_left (+) 0

let p2 lines = 
  let find_prior (s1, s2, s3) = let size = String.length s1 in let aux_char = ref '-' in
    for i = 0 to size - 1 do
      if String.contains s2 s1.[i] && String.contains s3 s1.[i] then aux_char := s1.[i];
    done;
    priority !aux_char in
  List.map find_prior lines |> List.fold_left (+) 0

let print_result = print_int

let () = read_lines "./inputs/i3.txt" |> process_input2 |> p2 |> print_result