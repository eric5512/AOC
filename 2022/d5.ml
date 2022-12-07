let read_lines file_name = let ic = open_in file_name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let process_input lines =
  let create_stacks lines = 
    let get_numbers = List.find (fun (x) -> Str.string_match (Str.regexp {| [0-9]+ |}) x 0) in
    let nums numbers = String.split_on_char ' ' numbers |> List.filter (fun x -> x <> "") |> List.length in
    let size = get_numbers lines |> nums in
    let aux_arr = Array.make size [] in
    let filter_boxes un_lines = List.filter (fun (line) -> String.contains line '[') un_lines |> List.rev in
    let populate_array line = 
      for i=0 to size - 1 do
        let aux = String.get line (1+i*4) in if aux <> ' ' then aux_arr.(i) <- aux::(aux_arr.(i));
      done in
    List.map populate_array (filter_boxes lines) |> ignore;
  aux_arr in
  let create_instructions lines = 
    let filter_instructions un_lines = List.filter (fun line -> String.contains line 'm') un_lines in
    let list2tuple = function 
      | [hd1;hd2;hd3] -> (hd1,hd2,hd3)
      | _ -> failwith "Incorrect formatting" in
    let extract_numbers line = String.sub line 5 (String.length line - 5) |> Str.split (Str.regexp {| from \| to |}) |> List.map int_of_string in
    filter_instructions lines |> List.map (fun x -> extract_numbers x |> list2tuple) in
  (create_stacks lines, create_instructions lines)

let take m = 
  let rec take_aux m acc l = 
    if m <= 0 then
      List.rev acc
    else match l with
    | hd::tl -> take_aux (m-1) (hd::acc) tl
    | [] -> List.rev acc in
  take_aux m []

let rec drop m l = 
  if m <= 0 then l
  else match l with
  | hd::tl -> drop (m-1) tl
  | [] -> []

let p1 (box_stack, instructions) = 
  let execute_instruction (many, src, dst) = 
    box_stack.(dst-1) <- List.rev_append (take many (box_stack.(src-1))) (box_stack.(dst-1));
    box_stack.(src-1) <- drop many (box_stack.(src-1));
    box_stack in
  List.map execute_instruction instructions |> ignore;
  box_stack

let p2 (box_stack, instructions) =
  let execute_instruction (many, src, dst) = 
    box_stack.(dst-1) <- (take many (box_stack.(src-1))) @ (box_stack.(dst-1));
    box_stack.(src-1) <- drop many (box_stack.(src-1));
    box_stack in
  List.map execute_instruction instructions |> ignore;
  box_stack

let rec print_list = function
  | hd::tl -> print_char hd
  | [] -> ()

let print_result arr =
  for i=0 to Array.length arr - 1 do
    print_list (arr.(i))
  done

let () = read_lines "./inputs/i5.txt" |> process_input |> p2 |> print_result