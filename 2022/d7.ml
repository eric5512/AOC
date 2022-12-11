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

let insert path node =
  let path_list = String.split_on_char '/' path |> List.rev |> List.tl |> List.rev in
  let rec aux dirs = function
  | Dir (name, lst) -> 
    let dir = List.hd dirs in 
    if List.length dirs = 1 && dir = name then
      Dir (name, node::lst)
    else
      if name = dir then
        Dir (name, List.map (aux (List.tl dirs)) lst)
      else
        Dir (name, lst)
  | _ as x -> x in
  aux path_list

let starts_with str prefix =
  let pref_len = String.length prefix in
  pref_len <= String.length str && String.sub str 0 pref_len = prefix

let dir_back s = 
  let aux = String.sub s 0 (String.rindex_from s (String.length s - 1) '/') in
    String.sub aux 0 (String.rindex_from aux (String.length aux - 1) '/') 
    |> (fun x -> x ^ "/")

let process_input lines =
  let aux act tree line =
    let len = String.length line in
    if starts_with line "$ cd" then
      if line = "$ cd .." then
        (dir_back act, tree)
      else if line = "$ cd /" then
        ("/", tree)
      else
        (act ^ (String.sub line 5 (len - 5)) ^ "/" , tree)
    else if starts_with line "$ ls" then
      (act, tree)
    else if starts_with line "dir" then
      (act, (insert act (Dir (String.sub line 4 (len - 4), [])) tree))
    else
      let [size; name] = String.split_on_char ' ' line in
      (act, (insert act (File (name, int_of_string size)) tree)) in
  List.fold_left (fun (nxt, tree) line -> aux nxt tree line) ("/", (Dir ("", []))) lines |> snd

module MS = Map.Make(String)

let make_map tree = 
  let dirs = ref MS.empty in
  let add_all n = List.map (fun x -> dirs := MS.update x (fun y -> Some (Option.value y ~default:0 + n)) !dirs) in
  let rec aux acc = function
  | File (_, size) -> add_all size acc |> ignore
  | Dir (name, content) -> List.map (fun x -> aux (((List.hd acc) ^ name ^ "/")::acc) x) content |> ignore in
  aux [""] tree; !dirs
  
let p1 tree = 
  make_map tree |> MS.filter (fun _ v -> v <= 100000) |> MS.to_seq |> Seq.map snd |> Seq.fold_left (+) 0

let p2 tree =
  let map = make_map tree in 
  let total = MS.find "/" map in
  let lis = MS.remove "" map |> MS.remove "/" |> MS.to_seq |> List.of_seq |> List.map snd in
  List.filter (fun v -> v >= total - 40000000) lis
  |> List.fast_sort Int.compare |> List.hd

let print_result = print_int

let () = read_lines "./inputs/i7.txt" |> process_input |> p2 |> print_result