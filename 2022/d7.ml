type node = 
  | File of string * int
  | Dir of string * node list

let read_lines file_name = let ic = open_in file_name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let insert_file fold_name (file_name, size) = 
  let rec aux = function
    | File _ as x -> x
    | Dir (name, lst) -> 
      if name = fold_name then
        Dir (name, (File (file_name, size))::lst)
      else
        Dir (name, List.map aux lst) in 
  aux

let insert_folder parent_name fold_name = 
  let rec aux = function
    | File _ as x -> x
    | Dir (name, lst) -> 
      if name = parent_name then
        Dir (name, (Dir (fold_name, []))::lst)
      else
        Dir (name, List.map aux lst) in
  aux

let get_prev_fold fold_name = 
  let rec aux prev = function
    | File _ -> ""
    | Dir (name, lst) -> 
      if name = fold_name then
        prev
      else
        List.fold_left (fun x y -> x ^ aux name y) "" lst in
  aux ""

let starts_with str prefix =
  let pref_len = String.length prefix in
  pref_len <= String.length str && String.sub str 0 pref_len  = prefix

let process_input lines = 
  let aux act tree line = 
    let len = String.length line in
    if starts_with line "$ cd" then
      if line = "$ cd .." then
        (get_prev_fold act tree, tree)
      else
        (String.sub line 5 (len - 5) , tree)
    else if starts_with line "$ ls" then
      (act, tree)
    else if starts_with line "dir" then
      (act, (insert_folder act (String.sub line 4 (len - 4)) tree))
    else
      let [size; name] = String.split_on_char ' ' line in
      (act, (insert_file act (name, int_of_string size) tree)) in
  List.fold_left (fun (nxt, tree) line -> aux nxt tree line) ("", (Dir ("/", []))) lines |> snd

module MS = Map.Make(String)

let p1 tree = 
  let dirs = ref MS.empty in
  let add_all n = List.map (fun x -> dirs := MS.update x (fun y -> Some (Option.value y ~default:0 + n)) !dirs) in
  let rec aux acc = function
    | File (_, size) -> add_all size acc |> ignore
    | Dir (name, content) -> List.map (fun x -> aux (name::acc) x) content |> ignore in
  aux [] tree; !dirs |> MS.filter (fun _ v -> v <= 100000) |> MS.to_seq |> List.of_seq |> List.map snd |> List.fold_left (+) 0

let p2 = ()

let print_result = print_int

let () = read_lines "./inputs/i7.txt" |> process_input |> p1 |> print_result