open Core

let get_range t id ~dir =
  match Map.closest_key t dir id with
    | Some (k, endpt) -> if id <= endpt then Some (k, endpt) else None
    | None -> None

let merge_ranges t id: _ Map.t =
  match get_range t id ~dir:`Less_than with
    | None -> t
    | Some (newlo, newhi) -> 
        let removed = Map.remove t id in
        let oldhi = Map.find_exn t id in
        let maxhi = Int.max newhi oldhi in
        Map.set removed ~key:newlo ~data:maxhi

let dedup_ranges ranges =
  let rangemap = Map.of_alist_reduce (module Int) ranges ~f:Int.max in
  List.fold_left (Map.keys rangemap) ~init:rangemap ~f:merge_ranges

let part1 ranges ids =
  let map_dedup = dedup_ranges ranges in
  let in_range id =
    match get_range map_dedup id ~dir:`Less_or_equal_to with
      | Some (_k, _v) -> 1;
      | None -> 0 in
  List.fold_left ids ~init:0 ~f:(fun acc a -> acc + in_range a)

let part2 ranges =
  let map_dedup = dedup_ranges ranges in
  Map.fold map_dedup ~init:0 ~f:(fun ~key ~data acc -> acc + data - key + 1)

let run filename =
  let file = In_channel.create filename in
  let lexbuf = Lexing.from_channel file in
  let parsed = Parse.parse_with_error Day5_parser.prog Day5_lexer.read lexbuf in
  printf "read %d ranges and %d ids\n" (List.length (fst parsed)) (List.length (snd parsed));
  printf "part 1: %d\n" (part1 (fst parsed) (snd parsed));
  printf "part 2: %d\n" (part2 (fst parsed))
