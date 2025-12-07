open Core

let find_start grid : int =
  fst @@ Array.findi_exn grid.(0) ~f:(fun _i -> function | `Start -> true | _ -> false)

let rec split_beams row beams : (int * int list) =
  match beams with
    | [] -> (0, [])
    | x :: xs ->
      match row.(x) with
        | `Split -> 
            let ct, next_beams = split_beams row xs in
            let next_beam_overlap =
              match List.hd next_beams with
                | Some bm -> bm = x + 1
                | None -> false in
            let new_beams = if next_beam_overlap then [x - 1] else [x - 1; x + 1] in
            (ct + 1, List.append new_beams next_beams)
        | `Space | `Start -> 
          let ct, next_beams = split_beams row xs in
          (ct, x :: next_beams)


let split_tree grid =
  let start = find_start grid in
  let add_row row_num = function
    | (split_ct, beams) -> 
      printf "beam 1: %d \n" (List.hd_exn beams);
      let newct, newbeams = split_beams grid.(row_num) [70] in
      printf "row %d: %d new splits\n" row_num newct;
      (newct + split_ct, newbeams) in
  let indices = List.tl_exn @@ List.init (Array.length grid) ~f:Fn.id in
  List.fold_left indices ~init:(0, [start]) ~f:(Fn.flip add_row)

let run filename =
  let file = In_channel.create filename in
  let lexbuf = Lexing.from_channel file in
  let parsed = Parse.parse_with_error Day7_parser.prog Day7_lexer.read lexbuf in
  let arrays = Array.of_list (List.map parsed ~f:Array.of_list) in
  printf "read %d lines\n" (Array.length arrays);
  let results = split_tree arrays in
  printf "part 1: %d splits\n" (fst results)
