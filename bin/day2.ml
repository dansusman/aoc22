let result = Aoc22.read_lines "day2.txt"

let score input func =
  let rec helper input func acc =
    match input with
    | [] -> acc
    | car :: cdr -> helper cdr func ((func car) + acc) in
  helper input func 0

(* Part 1 *)
let get_score1 s =
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

let () = print_endline "Part 1"
let () = score result get_score1 |> string_of_int |> print_endline

(* Part 2 *)
module RPS = struct
  type t =
    | Rock
    | Paper
    | Scissors

  let to_string = function
    | Rock -> "A"
    | Paper -> "B"
    | Scissors -> "C"
  
  let from_string = function
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | _ -> assert false

  let compare a b = compare (to_string a) (to_string b)
end

module RPSMap = Map.Make (RPS)
module StringMap = Map.Make (String)

let rps_map s = List.to_seq s |> RPSMap.of_seq
let str_map l = List.to_seq l |> StringMap.of_seq

(* They play x, I win/lose by playing y*)
let win = rps_map  [RPS.Rock, RPS.Paper;    RPS.Paper, RPS.Scissors; RPS.Scissors, RPS.Rock]
let lose = rps_map [RPS.Rock, RPS.Scissors; RPS.Paper, RPS.Rock;     RPS.Scissors, RPS.Paper]
let outcome_score = str_map ["X", 0; "Y", 3; "Z", 6]
let shape_score = rps_map [RPS.Rock, 1; RPS.Paper, 2; RPS.Scissors, 3]

let get_score2 s =
  let elf = String.sub s 0 1 |> RPS.from_string in
  let goal = String.sub s 2 1 in
  let result =
    match goal with
    | "X" (* I need to lose *) -> RPSMap.find elf lose
    | "Y" (* I need to draw *) -> elf
    | _   (* I need to win *)  -> RPSMap.find elf win in
  StringMap.find goal outcome_score + RPSMap.find result shape_score
  

let () = print_endline "Part 2"
let () = score result get_score2 |> string_of_int |> print_endline
