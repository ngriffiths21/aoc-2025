open Core

exception No_points
exception No_tree

module KDTree = struct
  type point = int * int * int

  type axis =
    | X
    | Y
    | Z

  type t = 
    | Leaf of axis
    | Node of axis * point * t * t
  
  let project (x, y, z): axis -> int = function
    | X -> x
    | Y -> y
    | Z -> z
  
  let next_axis = function
    | X -> Y
    | Y -> Z
    | Z -> X
  
  let split_list ax lst =
    let len = List.length lst in
    let compare x1 x2 = Int.compare (project x1 ax) (project x2 ax) in
    let sorted = List.sort ~compare lst in
    List.split_n sorted (len / 2)

  
  let of_list pts_list =
    let rec of_list' ax lst =
      match lst with 
      | [] -> Leaf ax
      | _ ->
        let left, right = split_list ax lst in
        let median, rest = match right with
        | m :: rest -> m, rest
        | [] -> raise No_points in
        let right_tree = of_list' (next_axis ax) rest in
        let left_tree = of_list' (next_axis ax) left in
        Node (ax, median, left_tree, right_tree) in
    of_list' X pts_list
end

let best_pt (pt1, pt1_dist) (pt2, pt2_dist) =
  if pt2_dist < pt1_dist then (pt2, pt2_dist)
  else (pt1, pt1_dist)

let calc_dist pt1 pt2: int =
  let dists : int list = List.map [KDTree.X; KDTree.Y; KDTree.Z] ~f:(fun a -> Int.pow (KDTree.project pt1 a - KDTree.project pt2 a) 2) in
  List.fold_left dists ~init:0 ~f:(+)

let find_closest tree box excepts = 
  let rec find_closest' tr bx nn nn_dist =
    match tr with
    | KDTree.Leaf _ -> nn, nn_dist
    | KDTree.Node (ax, pt, nd1, nd2) ->
      let hyperplane_dist = Int.abs @@ KDTree.project pt ax - KDTree.project box ax in
      if hyperplane_dist > nn_dist then
        let closer_subtree =
          if KDTree.project box ax > KDTree.project pt ax then nd2 (* box is on positive side of hyperplane *)
          else nd1 in
        find_closest' closer_subtree bx nn nn_dist
      else if Set.mem excepts pt then best_pt (find_closest' nd1 bx nn nn_dist) (find_closest' nd2 bx nn nn_dist)
      else let pt_dist = calc_dist pt box in
      if pt_dist < nn_dist then
        best_pt (find_closest' nd1 bx pt pt_dist) (find_closest' nd2 bx pt pt_dist)
      else best_pt (find_closest' nd1 bx nn nn_dist) (find_closest' nd2 bx nn nn_dist) in
  match tree with
  | KDTree.Leaf _ -> raise No_tree
  | KDTree.Node (_, pt, _, _) -> find_closest' tree box pt (calc_dist pt box)


let run filename =
  let file = In_channel.create filename in
  let lexbuf = Lexing.from_channel file in
  let parsed = Parse.parse_with_error Day8_parser.prog Day8_lexer.read lexbuf in
  printf "read %d input lines\n" (List.length parsed)
