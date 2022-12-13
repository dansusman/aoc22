let result = Aoc22.read_lines "day3.txt"

module CharSet = Set.Make (Char)

let set_of_char s = String.to_seq s |> CharSet.of_seq

let character_score c =
  let code = Char.code c in
  match c with
  | 'A' .. 'Z' -> code - 38
  | 'a' .. 'z' -> code - 96
  | _ -> assert false

let part1 acc s =
  let length = String.length s in
  let first = String.sub s 0 (length / 2) |> set_of_char in
  let second = String.sub s (length / 2) (length / 2) |> set_of_char in
  let intersect = CharSet.inter first second |> CharSet.choose in
  acc + (character_score intersect)

let () = print_endline "Part 1"
let () = List.fold_left part1 0 result |> Printf.printf "%d \n"

let part2 elves =
  let get_score x y z =
    let x = set_of_char x in
    let y = set_of_char y in
    let z = set_of_char z in
    let intersect = CharSet.inter x y |> CharSet.inter z |> CharSet.choose in
    character_score intersect in
  let rec fold3 func l acc =
    match l with
    | [] -> acc
    | car :: cadr :: caddr :: cdr -> fold3 func cdr (acc + (func car cadr caddr))
    | _ -> assert false in
  fold3 get_score elves 0

let () = print_endline "Part 2"
let () = part2 result |> Printf.printf "%d \n"
