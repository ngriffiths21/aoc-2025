open Core

let find_start grid : int =
  fst @@ Array.findi_exn grid.(0) ~f:(fun _i -> function | `Start -> true | _ -> false)

let rec split_beams row beams_left split_ct next_beam_row : (int * ((int * int) list)) =
  match beams_left with
    | [] -> (split_ct, List.rev next_beam_row)
    | (x, ct) :: xs ->
      match row.(x) with
        | `Split ->
          let new_beams =
            match next_beam_row with
              | (bm, c) :: lst ->
                if bm = x - 1 then (x + 1, ct) :: (x - 1, c + ct) :: lst
                else (x + 1, ct) :: (x - 1, ct) :: next_beam_row
              | [] -> [(x + 1, ct); (x - 1, ct)] in
          split_beams row xs (split_ct + 1) new_beams
        | `Space | `Start -> 
          let updated_row =
            match next_beam_row with
              | (bm, c) :: lst ->
                if bm = x then (x, c + ct) :: lst
                else (x, ct) :: next_beam_row
              | [] -> [(x, ct)] in
          split_beams row xs split_ct updated_row

let split_tree grid: int * (int * int) list =
  let start = find_start grid in
  let add_row row_num = function
    | (total, beams) -> 
      let newct, newbeams = split_beams grid.(row_num) beams 0 [] in
      (newct + total, newbeams) in
  let indices = List.tl_exn @@ List.init (Array.length grid) ~f:Fn.id in
  List.fold_left indices ~init:(0, [(start, 1)]) ~f:(Fn.flip add_row)

let run filename =
  let file = In_channel.create filename in
  let lexbuf = Lexing.from_channel file in
  let parsed = Parse.parse_with_error Day7_parser.prog Day7_lexer.read lexbuf in
  let arrays = Array.of_list (List.map parsed ~f:Array.of_list) in
  printf "read %d lines\n" (Array.length arrays);
  let part1 = split_tree arrays in
  printf "part 1: %d splits\n" (fst part1);
  let part2_beams = snd (split_tree arrays) in
  printf "part 2: %d worlds\n" (List.fold_left part2_beams ~init:0 ~f:(fun acc a -> acc + snd a))
