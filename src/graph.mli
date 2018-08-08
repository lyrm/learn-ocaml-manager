module Node : sig
  type t = string
  val compare : t -> t -> int
end

module Edge : sig
  type t = Node.t * Node.t
  val fst : t -> Node.t
  val snd : t -> Node.t
  val compare : t -> t -> int
end

type t

type path = Edge.t list

val add_node : Node.t -> t -> t
val remove_node : Node.t -> t -> t
val modify_node : Node.t -> Node.t -> t -> t

val add_edge : Edge.t -> t -> t
val remove_edge : Edge.t -> t -> t
val modify_edge : Edge.t -> Edge.t -> t -> t

val find_departure : t -> Node.t
val find_arrival : t -> Node.t

val longest_path : Node.t -> Node.t -> t -> Node.t list option
