open Printf

type maze_cell = | Free | Guard | Obstacle | Visited

type maze = maze_cell list list
type dir = | Up | Down | Left | Right

let parse_input filename : maze = 
  let ch = open_in filename in
  let read() = try Some(input_line ch) with End_of_file -> None in
  let rec loop maze = 
    match read() with
      | Some(line) -> 
        let row = String.to_seq line |> List.of_seq
        |> List.map (fun char -> 
          match char with
            | '.' -> Free
            | '^' -> Guard
            | '#' -> Obstacle
            | 'X' -> Visited
            | _ -> failwith"Invalid char when parsing"  
        ) in
       loop (maze @ [row])
      | None -> maze
  in
  loop []
;;

let find_guard maze : int * int = 
  List.fold_left
    (fun (found, row_idx) row ->
      match found with
        | None -> 
          (match List.find_index (fun x -> match x with | Guard -> true | _ -> false) row with
            | Some(x) -> Some(x, row_idx), row_idx + 1
            | None -> None, row_idx + 1)
        | Some(_) -> (found, row_idx + 1)
    )
    (None, 0)
    maze |> fst |> Option.get
;;

let next_pos (dir: dir) width height x y: (int * int) option = 
  match dir with
    | Up -> if y = 0 then None else Some(x, y - 1)
    | Down -> if y + 1 = height then None else Some(x, y + 1)
    | Left -> if x = 0 then None else Some(x - 1, y)
    | Right -> if x + 1 = width then None else Some(x + 1, y)
;;

let set maze x y sym = 
  List.mapi 
  (fun row_idx row -> 
    if row_idx = y then 
      List.mapi (fun col_idx cell -> if col_idx = x then sym else cell) row
  else row
  )
  maze
;;

let get maze x y = 
  (List.nth maze y |> List.nth) x

let rec move_guard maze width height ((x, y): int * int) (dir: dir): maze * (int * int) option =
  match next_pos dir width height x y with
    | Some(x', y')-> 
      (match get maze x' y' with
        | Free | Visited | Guard->
          let visited = set maze x y Visited in
          let guard = set visited x' y' Guard in
          move_guard guard width height (x', y') dir
        | Obstacle -> 
          maze, Some(x,y))
    | None -> set maze x y Visited, None
;;

let next_dir dir = 
  match dir with
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up
;;

let print maze = 
  List.iter 
    (fun row -> List.iter 
      (fun x -> (match x with
        | Free -> "."
        | Guard -> "^"
        | Obstacle -> "#"
        | Visited -> "X")
        |> print_string
      ) 
    row; print_endline"") 
  maze
;;

let rec escape maze width height pos (dir: dir): maze * (int * int) option = 
  match move_guard maze width height pos dir with
    | maze, Some(pos) -> escape maze width height pos (next_dir dir)
    | x -> x
;;

let rec is_in_loop maze width height pos dir visited: bool = 
  match move_guard maze width height pos dir with
    | maze, Some(pos) ->
       List.mem (pos, dir) visited || 
       is_in_loop maze width height pos (next_dir dir) ((pos, dir) :: visited)
    | x -> false
    ;;

let find_loops maze width height pos : int =
    List.mapi (fun row_idx row ->
      List.mapi (fun col_idx col -> 
        match get maze col_idx row_idx with
          | Free ->
          let with_obstacle = set maze col_idx row_idx Obstacle in
          if is_in_loop with_obstacle width height pos Up [] then 1 else 0
          | _ -> 0
        ) row
      ) maze 
    |> List.flatten
    |> List.fold_left (fun a b -> a + b) 0
  ;;

let count_visited maze : int = 
  List.fold_left
    (fun count row -> 
     List.map 
     (fun c -> match c with | Visited -> 1 | _ -> 0) 
     row 
     |> List.fold_left (fun a b -> a + b) count
      )
      0 maze
    ;;

let () = 
  let maze = parse_input "input/day6.txt" in
  let pos = find_guard maze in
  let width = List.length maze in
  let height = List.nth maze 0 |> List.length in
  print_endline (sprintf "Width: %d height: %d" width height);
  find_loops maze width height pos |> print_int




