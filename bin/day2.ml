let result = Aoc22.read_lines "day2.txt"

let get_score s =
  let elf =
  match String.sub s 0 1 with
  | "A" -> "X"
  | "B" -> "Y"
  | _ -> "Z"  in
  let mine = String.sub s 2 1 in
  let shape_score =
    match mine with
    | "X" -> 1
    | "Y" -> 2
    | _ -> 3 in
  let outcome_score =
    match elf, mine with
    | "X", "Y" (* win *) -> 6
    | "Y", "Z" (* win *) -> 6
    | "Z", "X" (* win *) -> 6
    | e, m when e = m (* draw *) -> 3
    | _ (* loss *) -> 0 in
  shape_score + outcome_score

let score input =
  let rec helper input acc =
    match input with
    | [] -> acc
    | car :: cdr -> helper cdr (get_score car + acc) in
  helper input 0

let () = score result |> string_of_int |> print_endline
