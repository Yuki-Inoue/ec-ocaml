
module type PARAM =
sig
  val connects : int
  val margin : int
end

module type S =
sig
  type node
  type action
  val play : node -> action -> node
  val random_move : node -> action
  val possible_moves : node -> action list
  val terminal : node -> bool
  val score : node -> int
  val initial : node
  val print_node : Format.formatter -> node -> unit
  val print_action : Format.formatter -> action -> unit
end

module Make : functor (Param : PARAM) -> S

(* attached morpoin *)
module T : S

(* detached morpoin *)
module D : S
