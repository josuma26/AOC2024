open Printf

let parse_input filename : char list list =
  let ch = open_in filename in
  let try_read () = try Some (input_line ch) with End_of_file -> None in
  let rec loop lines =
    match try_read() with
      | Some(line) -> loop (lines @ [List.init (String.length line) (String.get line)])
      | None -> lines
  in
  loop []
;;

let sum (l: int list): int = List.fold_left (fun a b -> a + b) 0 l;;

let range n m =
  List.init (m - n + 1) (fun i -> n + i)
;;

let get lines x y = 
  List.nth (List.nth lines y) x
;;


let rec check_direction lines (x: int) (y: int) (vx: int) (vy: int) (word: char list): bool = 
  match word with
    | [] -> true
    | first :: rest ->
      let height = List.length lines in
      let width = List.length (List.nth lines 0) in
      if x < 0 || y < 0 || x >= width || y >= height then
        false
      else
        (get lines x y) = first && (check_direction lines (x + vx) (y + vy) vx vy rest)
;;

let rec count_words_starting_from lines (word: char list) ((x, y): int * int) : int = 
  let directions = [(1,1); (1, -1)] in
  List.filter (fun (vx, vy) -> check_direction lines x y vx vy word) directions
   |> List.length
;;

let rec count_x_centerd_at lines word ((x, y): int * int): int = 
  match get lines x y with
    | 'A' -> 
      let directions = [[(1,1); (-1, -1)]; [(1, -1); (-1, 1)]] in
      let dirs = List.filter (fun directions -> 
        List.filter (fun (vx, vy) -> check_direction lines (x - vx) (y - vy) vx vy word) directions
        |> List.is_empty
        |> Bool.not
      ) directions in
      if List.length dirs = 2 then 1 else 0
    | _ -> 0
;;


let count_all_words (word: char list) (lines: char list list): int = 
  List.mapi 
    (fun row_idx row ->
      List.mapi (fun col_idx char ->
        count_x_centerd_at lines word (col_idx, row_idx)) row
      |> sum 
    )
  lines
  |> sum

let () = 
    parse_input "input/day4.txt" 
     |> count_all_words ['M'; 'A'; 'S']
     |> print_int 
