type locationIdLists = (int list) * (int list)

let parse_input (filename): locationIdLists = 
    let ch = open_in filename in
    let try_read () = try Some (input_line ch) with End_of_file -> None in
    let rec loop (left, right) = 
        match (try_read ()) with
            | Some(line) ->
                (match String.split_on_char ' ' line with
                    | [first; _; _; second] -> 
                        let fst_int = int_of_string first in
                        let snd_int = int_of_string second in
                        loop (left @ [fst_int], right @ [snd_int])
                    | x -> print_int (List.length x);  failwith"Invalid input")
            | None -> close_in ch; (left, right)
    in
    loop ([], []);;


let reconcile_location_id ((left, right): locationIdLists): int = 
    let left = List.sort Int.compare left in
    let right = List.sort Int.compare right in
    let rec loop left right = 
        match left, right with
            | f1::left, f2::right ->
                Int.abs (f1 - f2) + loop left right
            | [], [] -> 0
            | _ -> failwith"List should be equal length"
    in
    loop left right

let similarity_score ((left, right): locationIdLists): int = 
    let count_in_right n = List.fold_left (fun c x -> if x = n then c + 1 else c) 0 right in
    List.fold_right
        (fun l count -> l * (count_in_right l) + count)
        left
        0
;;

let () = 
    let location_ids = parse_input "input/day1-1.txt" in
    (* let distance = reconcile_location_id location_ids in *)
    let similarity_score = similarity_score location_ids in 
    print_endline (string_of_int similarity_score)