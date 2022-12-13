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
let best_three l =
  let rec best_three_h l (e1, e2, e3) =
    match l with
    | [] -> (e1, e2, e3)
    | car :: cdr ->
       best_three_h cdr
         (match e1, e2, e3 with
          | e1, e2, _  when car > e1 -> car, e1, e2
          | e1, e2, _  when car > e2 -> e1, car, e2
          | e1, e2, e3 when car > e3 -> e1, e2, car
          | _ -> e1, e2, e3) in
  let one, two, three = best_three_h l (0, 0, 0) in
  one + two + three

let () = print_endline (string_of_int (best_three (grouped result [])))
