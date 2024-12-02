
type levels = int list
type reports = levels list

let parse_input (filename): reports = 
  let ch = open_in filename in
  let try_read () = try Some (input_line ch) with End_of_file -> None in
  let rec loop report = 
      match (try_read ()) with
          | Some(line) ->
              (match String.split_on_char ' ' line with
                  | levels -> 
                      let levels = List.map int_of_string levels in
                      loop report @ [levels]
                  | _ ->  failwith"Invalid input")
          | None -> close_in ch; report
  in
  loop [];;

let rec increasing (level: levels): bool = 
  match level with
    | first :: second :: rest ->
      first < second && increasing (second :: rest)
    | _ :: [] -> true
    | _ -> true

let decrasing (level: levels): bool = increasing (List.rev level);;

let difference_in_bound (min: int) (max: int) (levels: levels): bool =
  let differences = match levels with
    | first :: rest -> List.fold_left
      (fun (prev, differences) curr -> (curr, Int.abs (prev - curr) :: differences))
      (first, [])
      rest
      |> snd
    | x -> x
  in
  List.for_all (fun diff -> diff >= min && diff <= max) differences

let level_safe (level: levels): bool = 
  (increasing level || decrasing level) && difference_in_bound 1 3 level;;

let level_safe_by_removing (levels: levels): bool = 
  let rec can_remove a b = 
    match b with
      | first :: b ->
        let loop_r = can_remove (a @ [first]) b in
        if level_safe (a @ b) then loop_r + 1 else loop_r
      | [] -> 0
  in
  can_remove [] levels > 0


let safe_levels (report: reports): int = 
  List.fold_left (fun a b -> a + b) 0 (List.map (fun l -> if level_safe_by_removing l then 1 else 0) report);;

let () = 
  let reports = parse_input "input/day2.txt" in
  print_int (safe_levels reports);;