let read_lines file_name = let ic = open_in file_name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let process_input inp = List.hd inp |> String.to_seq |> List.of_seq

let p1 = 
  let repeat v1 v2 v3 v4 = v1 = v2 || v1 = v3 || v1 = v4 || v2 = v3 || v2 = v4 || v3 = v4 in
  let rec aux n =
  function
    | hd1::hd2::hd3::hd4::tl ->
      if not (repeat hd1 hd2 hd3 hd4) then
        (n+4, (hd1, hd2, hd3, hd4))
      else
        aux (n+1) (hd2::hd3::hd4::tl)
    | _ -> failwith "Seq not found" in
  aux 0

let take n l =
  let rec aux acc n = function
    | hd::tl -> 
      if n <= 0 then
        List.rev acc
      else
        aux (hd::acc) (n-1) tl
    | [] -> List.rev acc in
  aux [] n l

module CS = Set.Make(Char);;

let p2 = 
  let size = 14 in
  let repeat l = 
    let set = CS.of_list l in
    (CS.cardinal set) <> size in
  let rec aux n = function
    | hd::tl ->
      if not (repeat (hd::(take (size-1) tl))) then
        n+size
      else
        aux (n+1) tl
    | _ -> failwith "Seq not found" in
  aux 0

let print_result (v1, v2, v3, v4) = print_char v1; print_char v2; print_char v3; print_char v4
let print_result n = print_int n

let () = read_lines "./inputs/i6.txt" |> process_input |> p2 |> print_result