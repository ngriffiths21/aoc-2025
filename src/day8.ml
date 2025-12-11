module KDTree = struct
  module Point = struct
    module T = struct
      type t = int * int * int
      (* comparison functions, but account for different axes *)
  end

  type axis =
    | X
    | Y
    | Z

  type t = 
    | Leaf of axis
    | Node of axis * point * t * t
  
  let project (x, y, z) = function
    | X -> x
    | Y -> y
    | Z -> z
  
  let next_axis ax = function
    | X -> Y
    | Y -> Z
    | Z -> X
  
  let rec add tree pt =
    match tree with
    | Leaf axis -> Node (pt, Leaf (next_axis axis), Leaf (next_axis axis))
    | Node (axis, split, tree1, tree2) -> 
      if project pt axis > project split axis then
        Node (axis, split, tree1, add tree2 pt)
      else Node (axis, split, add tree1 pt, tree2)
  
  let split_list ax lst =

  
  let of_list pts_list =
    let rec of_list' ax lst =
      match pts_list with 
      | [] -> Leaf ax
      | _ ->
        let median, left, right = split_list ax lst in
        let right_tree = of_list' (next_axis ax) right in
        let left_tree = of_list' (next_axis ax) left in
        Node (ax, median, left_tree, right_tree)
end

(* notes on of_list impl

I'm using a non-tail-recursive structure here which might be space inefficient.
I could imagine another strategy where each call to of_list':

- Adds the median to the tree
- Consumes that list from the queue but also pushes the two resulting lists (left & right of the median) and their axes

I think this is the way to do tail recursion. The queue saves space because at each iteration we can remove the previous iteration's
data. It sort of turns the problem into a process of sorting into order by medians, tracking axis changes, and then doing the quick "add tree" op.
Here the whole tree is passed to each iteration, we aren't building it up recursively.

An alternative way to do it more cheaply is to build up recursively, which is in general not a space problem -- because
the whole tree needs to be built anyway, so it's not that different to build it incrementally during function calls. The problem of course
is if there's too much data on the stack.

So to remove this extra stack data we can just maintain the whole list of points, and pass constraints instead of copies of the list.

Another way: at each iteration, first calculate the shallowest leaf, then insert the median element of those that fit there.
In other words at each iteration, pass the whole outstanding list, then descend the tree breadth first until a leaf is found;
each time we pass to a lower depth we also want to shorten the list, but breadth first means this won't be tail recursive. So I
think this fails.

Another trade off would be maintaining 3 sorted lists, one along each axis, and popping the median from *all* at each iteration.

I'm not sure which is best. In the non-tail-recursive approach I already wrote, yes, we make copies of the list
but once the list is copied, the old ref is nullified. The only thing that grows is the tree itself, which would just
grow on the stack instead in the tail recursive version.

Basically, the tail recursive approach may allow a bit of optimization but I'm not convinced
that the non-tail-recursive way is significantly more overhead. In particular,
the non-tail-recursive structure costs some constant multiple of the tree itself in terms of memory;
the final chain of returns is kind of like moving some data from stack to heap and deleting some data,
which is very different from a reduce operation where the size of the result is much smaller than the input list.

*)