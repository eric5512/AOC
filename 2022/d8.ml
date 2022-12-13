let read_lines file_name = let ic = open_in file_name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let array_max = Array.fold_left max Int.min_int

let process_input lines = 
  List.map (fun x -> Array.init (String.length x) (String.get x) 
            |> Array.map (fun y -> int_of_char y - 48)) lines
  |> List.to_seq |> Array.of_seq

let p1 input =
  let n = Array.length input in
  let m = Array.length input.(0) in
  let transposed =
    let mat = Array.make_matrix m n 0 in
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        mat.(j).(i) <- input.(i).(j)
      done
    done;
    mat in
  let slice_max arr beg len = array_max (Array.sub arr beg len) in
  let count = ref 0 in
  for i = 1 to n - 2 do
    let row = input.(i) in
    for j = 1 to m - 2 do
      let col = transposed.(j) in
      let value = input.(i).(j) in
      if   slice_max row 0 j >= value
        && slice_max row (j + 1) (n - j - 1) >= value
        && slice_max col 0 i >= value
        && slice_max col (i + 1) (m - i - 1) >= value then
        count := !count + 1
    done
  done;
  n * m - !count

let p2 = ()

let print_result = print_int

let () = read_lines "./inputs/test.txt" |> process_input |> p1 |> print_result