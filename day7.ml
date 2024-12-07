open Printf

let parse_input filename : (int * int list) list =
  let ch = open_in filename in
  let read() = try Some(input_line ch) with End_of_file -> None in
  let rec loop equations = 
    match read() with
      | Some(line) -> (
        match String.split_on_char ':' line with
          | [target; values] -> 
            (match String.split_on_char  ' ' values with
              | _::values ->
                loop (equations @ [(int_of_string target, List.map int_of_string values)])
              | _ -> failwith"Bad input")
          | _ -> failwith"Bad input")
      | None -> equations
  in
  loop []
;;

let rec equation_valid ((target, values): int * int list): bool = 
  let concat a b = (string_of_int a) ^ (string_of_int b) |> int_of_string in 
  match values with
    | [] ->  target = 0
    | [first] -> 
      first = target
    | first :: second :: rest ->
      equation_valid (target, first + second :: rest) ||
      equation_valid (target , first * second :: rest) ||
      equation_valid (target, concat first second :: rest)
;;

let valid_equations equations = List.filter equation_valid equations;;

let print_eq ((target, values): int * int list): string = 
  sprintf "%d %s" target (List.fold_left (fun a b -> (string_of_int b) ^ " " ^ a) "" values)
;;
let () = 
  let equations = parse_input "input/day7.txt" in
  let valid_equations = valid_equations equations in
  List.map (fun (target, _) -> target) valid_equations  |> List.fold_left (fun a b -> a + b) 0
  |> print_int

