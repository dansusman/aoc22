let result = Aoc22.read_lines "day1.txt"

let rec grouped l acc =
  match l with
  | [] -> acc
  | "" :: cdr -> grouped cdr (0 :: acc)
  | calories :: cdr -> 
     let cal = int_of_string calories in
     grouped cdr
       (match acc with
        | [] -> [cal]
        | car :: cdr -> (car + cal) :: cdr)

(* Part 1 *)
let max l = List.fold_left (fun a b -> max a b) 0 l
let () = print_endline (string_of_int (max (grouped result [])))

(* Part 2 *)
