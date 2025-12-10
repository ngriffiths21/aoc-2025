open Core

let adjacents = [ (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1) ]

let all_rolls grid pos =
  let adjacent_roll grid pos dir =
    try grid.(fst pos + fst dir).(snd pos + snd dir) with
      Invalid_argument _msg -> 0 in
      (* overflow, just skip *)
  List.fold_left adjacents ~init:0 ~f:(fun acc a -> acc + adjacent_roll grid pos a)

let test_input = [| [| 0; 0; 1 |]; [| 0; 0; 0; |]; [| 1; 1; 1 |]|]

let%test_unit "all_rolls" =
  [%test_eq:int] (all_rolls test_input (1,1)) 4

let%test_unit "all_rolls at edge" =
  [%test_eq:int] (all_rolls test_input (1,0)) 2

(* part 2 design

- Recurse over grid
- Ideally, construct lazy list of indices, check each
- Then if roll is "accessible," remove from array and cons "left" and "above" rolls onto index stack

basically requirement is that we specify limits based on grid length, and get indices one at a time
*)
let accessible grid pos =
    if grid.(fst pos).(snd pos) <> 1 then 0
    else if all_rolls grid pos < 4 then 1
    else 0

let next_idx dims idx : (int * int) option =
  let new_idx =
    if (fst idx) >= (fst dims) then (0, (snd idx) + 1)
    else (fst idx + 1, snd idx) in
  if (snd new_idx) >= (snd dims) then None
  else Some new_idx

let rec part2 grid count =
  let new_ct = ref 0 in
  for i = 0 to Array.length grid - 1 do
    for j = 0 to Array.length grid.(0) - 1 do
      if accessible grid (i, j) = 1 then
        (new_ct := !new_ct + 1;
        grid.(i).(j) <- 0)
    done
  done;
  if !new_ct = 0 then count
  else part2 grid (!new_ct + count)

let part1 grid =
  let sum = ref 0 in
  for i = 0 to Array.length grid - 1 do
    for j = 0 to Array.length grid.(0) - 1 do
      sum := !sum + accessible grid (i,j)
    done
  done;
  !sum

let run filename =
  let file = In_channel.create filename in
  let lexbuf = Lexing.from_channel file in
  let input = Parse.parse_with_error Day4_parser.prog Day4_lexer.read lexbuf in
  let arraygrid = List.map input ~f:Array.of_list |> Array.of_list in
  printf "read %d rows\n" (Array.length arraygrid);
  printf "part 1: %d\n" (part1 arraygrid);
  printf "part 2: %d\n" (part2 arraygrid 0)