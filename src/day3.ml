open Core

let rec find_first_digit ~trunc arr max_ptr start : int =
  (* never use last digit *)
  if start = Array.length arr - trunc then max_ptr
  else if arr.(start) > arr.(max_ptr) then find_first_digit arr start (start + 1) ~trunc
  else find_first_digit arr max_ptr (start + 1) ~trunc


let%test_unit "finds max in middle with dup" =
  [%test_eq:int] (find_first_digit [|1; 3; 3; 2; 1 |] 0 0 ~trunc:1) 1
let%test_unit "no max at last digit" =
  [%test_eq:int] (find_first_digit [|1; 2; 3; 2; 4 |] 0 0 ~trunc:1) 2


let rec find_digits arr ndigits start : int list =
  if ndigits = 0 then []
  else let newdigit = find_first_digit arr start start ~trunc:(ndigits - 1) in
  newdigit :: find_digits arr (ndigits - 1) (newdigit + 1)

let%test_unit "finds tw0 digits middle" =
  [%test_eq:int list] (find_digits [|1; 3; 8; 8; 3 |] 2 0) [2; 3]
let%test_unit "finds 98 end" =
  [%test_eq:int list] (find_digits [|1; 3; 9; 8 |] 2 0) [2; 3]

let find_joltage arr nbatt =
  let digits = find_digits arr nbatt 0 in
  List.fold_left digits ~init:0 ~f:(fun acc a -> acc*10 + arr.(a))

let%test_unit "finds joltage 88" =
  [%test_eq:int] (find_joltage [|1; 3; 8; 8; 3 |] 2) 88

let total_joltage arrays nbatt =
  List.fold_left arrays ~init:0 ~f:(fun acc a -> acc + find_joltage a nbatt)

let run filename =
  let file = In_channel.create filename in
  let lexbuf = Lexing.from_channel file in
  let input = Day3_parser.prog Day3_lexer.read lexbuf in
  let arrays = List.map input ~f:Array.of_list in
  printf "read %d input lines\n" (List.length input);
  printf "part 1: %d\n" (total_joltage arrays 2);
  printf "part 2: %d\n" (total_joltage arrays 12)