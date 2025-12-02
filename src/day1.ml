open Core

let parse_input lexbuffer =
  match Parse.parse_with_error Day1_parser.prog Day1_lexer.read lexbuffer with
    | Some x -> x
    | None -> []

let rec count_zeroes nums curr_pos count =
  match nums with
    | [] -> count
    | x :: xs ->
        let new_count = if curr_pos = 0 then (count + 1) else count in
        count_zeroes xs ((x + curr_pos) mod 100) new_count

let reduce curr_pos offset =
  let overflowed = curr_pos + offset in
  let newpos = overflowed mod 100 in
  let pluspos = if newpos < 0 then newpos + 100 else newpos in
  let passes = 
    match (curr_pos = 0, overflowed mod 100 = 0, overflowed > 0) with
      | (true, _, _) -> abs (offset / 100)
      | (false, false, true) -> abs (overflowed / 100)
      | (false, false, false) -> abs (overflowed / 100) + 1
      | (false, true, _) -> abs (offset / 100) + 1 in
  (passes, pluspos)

let rec count_all_zeroes nums curr_pos count =
  match nums with
    | [] -> count
    | x :: xs ->
        let (passes, new_pos) = reduce curr_pos x in
        count_all_zeroes xs new_pos (count + passes)

let%test_unit "basic" =
  [%test_eq: int * int] (reduce 10 10) (0, 20)
let%test_unit "turn once" =
  [%test_eq: int * int] (reduce 1 99) (1, 0)
let%test_unit "left once" =
  [%test_eq: int * int] (reduce (-1) (-99)) (1, 0)
let%test_unit "0 to 100" =
  [%test_eq: int * int] (reduce 0 100) (1, 0)
let%test_unit "0 to -100" =
  [%test_eq: int * int] (reduce 0 (-100)) (1, 0)
let%test_unit "1 to 100" =
  [%test_eq: int * int] (reduce 1 (99)) (1, 0)
let%test_unit "1 to 102" =
  [%test_eq: int * int] (reduce 1 (101)) (1, 2)
let%test_unit "1 to -2" =
  [%test_eq: int * int] (reduce 1 (-3)) (1, 98)
let%test_unit "1 to -100" =
  [%test_eq: int * int] (reduce 1 (-101)) (2, 0)
let%test_unit "1 to -101" =
  [%test_eq: int * int] (reduce 1 (-102)) (2, 99)
let%test_unit "0 to 200" =
  [%test_eq: int * int] (reduce 0 200) (2, 0)

let run filename =
  let file = In_channel.create filename in
  let lexbuffer = Lexing.from_channel file in
  let input = parse_input lexbuffer
  in printf "first part: %d" (count_zeroes input 50 0);
    printf "second part: %d" (count_all_zeroes input 50 0)
