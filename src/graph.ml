(*
module type NODE = sig
  type t
  type label = string
  val compare : t -> t -> int
  val new_node : label -> t
end

module Node : NODE = struct
  type nodeid = int
  type label = string
  type t = nodeid * string
  let next_id = ref 0
  let get_next_id () = incr next_id; !next_id
  let compare (id1, _) (id2, _) = compare id1 id2
  let new_node label = (get_next_id (), label)
end
*)

module Node = struct
  type t = string
  let compare = compare
end

module Edge = struct
  type t = Node.t * Node.t
  let fst (n, _) = n
  let snd (_, n) = n
  let compare = compare
end

module Nodes = Set.Make (Node)
module Edges = Set.Make (Edge)
type t = Nodes.t * Edges.t

type path = Edge.t list

let add_node n (ns, es) = Nodes.add n ns
let remove_node n (ns, es) = Nodes.remove n ns
let modify_node n n' (ns, es) = Nodes.remove n ns |> Nodes.add n'
let find_node n (ns, es) = Nodes.find n ns

let add_edge e (ns, es) = Nodes.add e es
let remove_edge e (ns, es) = Nodes.remove e es
let modify_edge e e' (ns, es) = Nodes.remove e es |> Nodes.add e'

let find_departure (ns, es) = Nodes.min_elt ns
let find_arrival (ns, es) = Nodes.max_elt ns

let successors_of n (ns, es) =
  let paths = Edges.filter (fun e -> n = Edge.fst e) es in
  Edges.fold (fun e acc -> Nodes.add (Edge.snd e) acc) paths Nodes.empty

let predecessors_of n (ns, es) =
  let paths = Edges.filter (fun e -> n = Edge.snd e) es in
  Edges.fold (fun e acc -> Nodes.add (Edge.fst e) acc) paths Nodes.empty

let longest_path departure arrival g =
  let exception PathFound of Node.t list in
  let rec one_step d current_path =
    let successors = successors_of d g in
    if Nodes.mem arrival successors then
      raise (PathFound (List.rev (arrival :: current_path)))
    else
      let path = d :: current_path in
      let new_successors =
        Nodes.filter (fun x -> not (List.mem x path)) successors in
      Nodes.iter
        (fun next_node -> one_step next_node path) new_successors
  in
  try one_step departure [departure]; None
  with PathFound p -> Some p

let independant_vertices (ns, es as g) =
  let empty_successors n = Nodes.is_empty (successors_of n g) in
  let empty_predecessors n = Nodes.is_empty (predecessors_of n g) in
  Nodes.filter (fun n -> empty_successors n && empty_predecessors n) ns

let sort_nodes (ns, es) = Nodes.elements ns
