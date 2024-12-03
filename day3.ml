
type mul = | Mul of int * int

type state =
  | Start 
  | Looking of char  
  | HaveLeft of int
  | HaveMul of mul 
  | Cond of char 
  | CondClose of bool
  | CondDone of bool
  | Done

let is_digit (char: char): bool = 
  List.mem char ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9']
;;

let parse_int (ch: in_channel) (terminator: char): int option = 
  let try_read_char () = try Some (input_char ch) with End_of_file -> None in
  let rec loop previous : string option = 
    match try_read_char() with
      | Some(char) -> 
        if is_digit char then loop (previous ^ (Char.escaped char) )
        else if (char = terminator) then Some(previous) else None
      | None -> Some previous
      in 
    (loop "")
    |> Option.map int_of_string
  ;;


let parse_input filename: mul list = 
  let ch = open_in filename in
  let try_read_char () = try Some (input_char ch) with End_of_file -> None in
  let rec loop muls state enabled = 
    let state = match state with
      | Start | Looking _ | Cond _ | CondClose _ -> (
        let char = try_read_char() in
        (match state, char with
          | Start, Some('m')
          | Looking('m'), Some('u')
          | Looking('u'), Some('l') -> Looking(Option.get char)
          | Looking('l'), Some('(') ->
            (match parse_int ch ',' with
              | Some(left) -> HaveLeft(left) | None -> Start)
          | Start, Some('d') -> Cond('d')
          | Cond('d'), Some('o') -> Cond('o')
          | Cond('o'), Some('(') -> CondClose(true)
          | Cond('o'), Some('n') -> Cond('n')
          | Cond('n'), Some('\'') -> Cond('\'')
          | Cond('\''), Some('t') ->  Cond('t')
          | Cond('t'), Some('(') -> CondClose(false)
          | CondClose(e), Some(')') ->CondDone(e)
          | _, Some(_) -> Start
          | _, None -> Done))
      | HaveLeft(left) ->
        (match parse_int ch ')' with
          | Some(right) -> HaveMul(Mul(left, right)) | None -> Start)
      | HaveMul _ -> Start
      | CondDone _ -> Start
      | Done -> Done
    in
    let muls = match state with
      | HaveMul(mul) when enabled -> muls @ [mul]
      | _ -> muls
    in
    match state with
      | Done -> muls
      | CondDone enabled -> loop muls state enabled
      | _ -> loop muls state enabled
  in
    loop [] Start true;;

let multiply (m: mul): int = 
  match m with
    | Mul(left, right) -> left * right
;;

let sum_muls (muls: mul list): int = 
  List.map multiply muls |> (List.fold_left (fun a b -> a + b) 0)
;;

let () = 
  let muls = parse_input "input/day3.txt" in
  List.iter (fun m ->
    match m with | Mul(a, b) -> print_endline (Printf.sprintf "%d * %d" a b)
    ) muls;
  print_int (sum_muls muls)