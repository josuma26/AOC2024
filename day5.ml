open Printf

type ordering_rules = (int * int) list
type update_pages = int list list

type associated_orderings = (int * ((int list) * (int list))) list

let parse_input filename : ordering_rules * update_pages = 
  let ch = open_in filename in
  let read_line () = try Some(input_line ch) with End_of_file -> None in
  let rec ordering_rules rules = 
    match read_line() with
      | Some("") -> rules
      | Some(line) -> (match String.split_on_char '|' line with
        | [a;b] -> ordering_rules (rules @ [(int_of_string a,int_of_string b)])
        | _ -> failwith"Bad input, expected pair")
      | _ -> failwith"Unexpected input"
  in
  let rules = ordering_rules [] in
  let rec update_pages updates = 
    match read_line() with
      | Some(line) -> (
        let pages =  String.split_on_char ',' line |> List.map int_of_string in
        update_pages (updates @ [pages]))
      | _ -> updates
  in
  rules, update_pages []
;;

let associate_orderings (rules: ordering_rules) : associated_orderings = 
  List.fold_left
    (fun associated (before, after) ->
      let prev_assoc = 
        match List.assoc_opt before associated with
          | Some(blist, alist) ->
            (before, (blist, after :: alist))
          | None -> (before, ([], [after]))
      in
      let succ_assoc = 
        match List.assoc_opt after associated with
          | Some(blist, alist) ->
            (after, (before :: blist, alist))
          | None -> (after, ([before], []))
      in
      prev_assoc :: succ_assoc :: associated
    )
    []
    rules
;;

let print_list l = (List.fold_left (fun a b -> a ^ "," ^ (string_of_int b)) "" l);;

let valid_udpate (associated: associated_orderings) (update : int list): bool = 
  let rec valid_upd_acc before after = 
    match after with
      | [] -> true
      | first::after -> 
        let (allowed_before, allowed_after) = List.assoc first associated in
        List.for_all (fun b -> List.mem b allowed_before) before
        && List.for_all (fun a -> List.mem a allowed_after) after
        && valid_upd_acc (before @ [first]) after

  in
  valid_upd_acc [] update 
;;
let valid_updates (associated: associated_orderings) (updates: update_pages) : update_pages = 
  List.filter (valid_udpate associated) updates
;;

let invalid_updates (associated: associated_orderings) (updates: update_pages) : update_pages = 
  List.filter (fun x -> Bool.not (valid_udpate associated x)) updates
;;

let order_pages (associated: associated_orderings) (updates: update_pages) : update_pages = 
  let sort  = List.sort (fun x y -> 
    let (xbefore, xafter) = List.assoc x associated in
    let (ybefore, yafter) = List.assoc x associated in
    if (List.mem x ybefore || List.mem y xafter) then -1 else 1
  ) in
  List.map sort updates
;;

let middle_values (updates: update_pages): int list = 
  List.map (fun u ->
    List.nth u (List.length u / 2)  
  ) updates
;;

let () = 
  let rules, updates = parse_input "input/day5.txt" in
  let associated = associate_orderings rules in
  let invalid = invalid_updates associated updates in
  let ordered = order_pages associated invalid in 
  List.iter (fun x -> print_list x |> print_endline) ordered;

  middle_values ordered |> List.fold_left (fun a b -> a + b) 0
  |> print_int
